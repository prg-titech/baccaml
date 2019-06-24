open Utils
open Std
open Base
open Jit
open Jit_env
open Internal

exception Jit_compilation_failed

module Method_prof = Make_prof(struct let threshold = 100 end)

module Trace_prof = Make_prof(struct let threshold = 100 end)

module Trace_name : sig
  type t = Trace_name of string

  val gen : [< `Meta_tracing | `Meta_method] -> t
  val value : t -> string
end = struct
  type t = Trace_name of string

  let counter = ref 0

  let gen typ =
    let mark = match typ with
        `Meta_tracing -> "tj"
      | `Meta_method -> "mj"
    in
    let name = "trace" ^ mark ^ string_of_int !counter in
    incr counter;
    Trace_name (Id.genid name)

  let value = function Trace_name s -> s
end

module Internal_conf = struct
  let size = 2000000

  (* TODO: specify extenally *)
  let greens = ["pc"; "bytecode"]

  let bc_tmp_addr = 0

  let st_tmp_addr = 1000
end

module Debug = struct
  open Utils
  let print_trace trace =
    if !Log.log_level = `Debug then
      Asm.print_fundef trace
    else ()

  let print_arr ?notation:(nt = None) f arr =
    if !Log.log_level = `Debug then
      let str = Array.string_of_array f arr in
      match nt with
      | Some s -> Printf.printf "%s %s\n" s str
      | None -> Printf.printf "%s\n" str
    else ()
end

let file_open () =
  match !Config.file_name with
  | Some name -> open_in name
  | None -> failwith "argument is not specified."

let get_ir_addr args name =
  args
  |> List.find (fun a -> String.get_name a = name)
  |> String.get_extension |> int_of_string


let get_so_name : string -> string =
  fun name ->
    let ic = Unix.open_process_in "uname" in
    let uname = input_line ic in
    let () = close_in ic in
    if uname = "Linux" then
      "lib" ^ name ^ ".so"
    else if uname = "Darwin" then
      "lib" ^ name ^ ".dylib"
    else
      raise Exit

let make_reg prog args sp =
  Jit_env.(
    let reg = Array.make Internal_conf.size (Red 0) in
    let Asm.{args; body= t} = Fundef.find_fuzzy prog "interp" in
    Asm.fv t @ args
    |> List.iteri (fun i a ->
           if List.mem (String.get_name a) Internal_conf.greens then reg.(i) <- Green 0
           else reg.(i) <- Red 0 ) ;
    reg)

let make_mem ~bc_addr ~st_addr bytecode stack =
  Jit_env.(
    let mem = Array.make Internal_conf.size (Green 0) in
    bytecode
    |> Array.iteri (fun i a -> mem.(bc_addr + (4 * i)) <- Jit_env.Green a) ;
    stack
    |> Array.iteri (fun i a -> mem.(st_addr + (4 * i)) <- Jit_env.Red a) ;
    mem)

let compile_dyn trace_name =
  let asm_name = trace_name ^ ".s" in
  let so = get_so_name trace_name in
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  if uname = "Linux" then
    Printf.sprintf
      "gcc -m32 -g -DRUNTIME -o %s %s -shared -fPIC -ldl"
      so asm_name
    |> Unix.system
    |> function
        Unix.WEXITED (i) when i = 0 -> Ok trace_name
      | _ -> Error (Jit_compilation_failed)
  else if uname = "Darwin" then
    Printf.sprintf "gcc -m32 -g -o %s -dynamiclib %s" so asm_name
    |> Unix.system
    |> function
        Unix.WEXITED (i) when i = 0 -> Ok trace_name
      | _ -> Error (Jit_compilation_failed)
  else
    Error (Jit_compilation_failed)

let emit_dyn : out_channel -> [`Meta_method | `Meta_tracing] -> Asm.fundef list -> unit =
  fun oc typ traces ->
  (try
     traces
     |> List.iter (fun trace ->
            Simm.h trace
            |> RegAlloc.h
            |> Emit.Interop.h oc typ)
   with e -> close_out oc; raise e)

type runtime_env =
  { bytecode: int array
  ; stack: int array
  ; pc: int
  ; sp: int
  ; bc_ptr: int
  ; st_ptr: int }

let get_id elem =
  List.find (fun arg -> String.get_name arg = elem)

let filter typ = match typ with
    `Red ->
     List.filter (fun a -> (List.mem (String.get_name a) !Config.reds))
  | `Green ->
     List.filter (fun a -> List.mem (String.get_name a) Internal_conf.greens)

let jit_method {bytecode; stack; pc; sp; bc_ptr; st_ptr} prog =
  Debug.print_arr string_of_int bytecode;
  let prog = Jit_annot.annotate `Meta_method prog in
  let Asm.{args; body} = Fundef.find_fuzzy prog "interp" in
  let reg = make_reg prog args sp in
  let mem =
    Internal_conf.(make_mem ~bc_addr:bc_tmp_addr ~st_addr:st_tmp_addr bytecode stack)
  in
  let pc_method_entry = pc in
  let pc_ir_addr = get_ir_addr args "pc" in
  let sp_ir_addr = get_ir_addr args "sp" in
  let bc_ir_addr = get_ir_addr args "bytecode" in
  let st_ir_addr = get_ir_addr args "stack" in
  let module E = Jit_env in
  reg.(pc_ir_addr) <- E.Green pc_method_entry ;
  reg.(sp_ir_addr) <- E.Red sp ;
  reg.(bc_ir_addr) <- E.Green Internal_conf.bc_tmp_addr ;
  reg.(st_ir_addr) <- E.Red Internal_conf.st_tmp_addr ;
  let module JM = Jit_method in
  let trace_name = Trace_name.gen `Meta_method in
  let env =
    E.create_env
      ~trace_name:(Trace_name.value trace_name)
      ~red_names:(!Config.reds)
      ~index_pc:(List.index (get_id "pc" args) args)
      ~merge_pc:pc_method_entry in
  let trace = JM.run prog reg mem env |> Jit_elim.elim_fundef ~i:1 in
  Debug.print_trace trace;
  flush_all ();
  let oc = open_out (Trace_name.value trace_name ^ ".s") in
  try
    emit_dyn oc `Meta_method [trace]; close_out oc;
    compile_dyn (Trace_name.value trace_name)
  with e ->
    close_out oc; raise e

let jit_tracing {bytecode; stack; pc; sp; bc_ptr; st_ptr} prog =
  let prog = Jit_annot.annotate `Meta_tracing prog in
  let Asm.{args; body} = Fundef.find_fuzzy prog "interp" in
  let reg = make_reg prog args sp in
  let mem =
    Internal_conf.(make_mem ~bc_addr:bc_tmp_addr ~st_addr:st_tmp_addr bytecode stack)
  in
  let pc_ir_addr = get_ir_addr args "pc" in
  let sp_ir_addr = get_ir_addr args "sp" in
  let bc_ir_addr = get_ir_addr args "bytecode" in
  let st_ir_addr = get_ir_addr args "stack" in
  reg.(pc_ir_addr) <- Green pc ;
  reg.(sp_ir_addr) <- Red sp ;
  reg.(bc_ir_addr) <- Green Internal_conf.bc_tmp_addr ;
  reg.(st_ir_addr) <- Red Internal_conf.st_tmp_addr ;
  let module JT = Jit_tracing in
  let trace_name = Trace_name.gen `Meta_tracing in
  let env =
    Jit_env.create_env
      ~index_pc:(
        let pc_id = List.find (fun arg -> String.get_name arg = "pc") args in
        List.index pc_id args)
      ~merge_pc:pc
      ~trace_name:(Trace_name.value trace_name)
      ~red_names:(!Config.reds)
  in
  let trace = JT.run prog reg mem env in
  Debug.print_trace trace;
  let oc = open_out (Trace_name.value trace_name ^ ".s") in
  try
    emit_dyn oc `Meta_tracing [trace];
    close_out oc;
    compile_dyn (Trace_name.value trace_name)
  with e ->
    close_out oc; raise e

let exec_dyn_arg2 ~name ~arg1 ~arg2 =
  Dynload_stub.call_arg2
    ~lib:("./" ^ get_so_name name)
    ~func:(String.split_on_char '.' name |> List.hd)
    ~arg1:arg1 ~arg2:arg2

let exec_dyn_arg3 ~name ~arg1 ~arg2 ~arg3 =
  Dynload_stub.call_arg3
    ~lib:("./" ^ get_so_name name)
    ~func:(String.split_on_char '.' name |> List.hd)
    ~arg1:arg1 ~arg2:arg2 ~arg3:arg3

let jit_exec pc st_ptr sp =
  if !Config.jit_flag = `Off then ()
  else
    match Trace_prof.find_opt pc with
    | Some (tname) ->
       Printf.printf "[tj] executing %s at pc: %d ...\n" tname pc;
       let s = Unix.gettimeofday () in
       exec_dyn_arg2 ~name:tname ~arg1:st_ptr ~arg2:sp |> ignore;
       let e = Unix.gettimeofday () in
       Printf.printf "[tj] ellapsed time: %f ms\n" ((e -. s) *. 1000.0);
       flush stdout
    | None -> ()

let jit_exec_method pc st_ptr sp =
  if !Config.jit_flag = `Off then ()
  else
    let ic = file_open () in
    let prog = Lexing.from_channel ic |> Util.virtualize |> Jit_annot.annotate `Meta_method in
    let intep = Fundef.find_fuzzy prog "interp" in
    ()

let jit_tracing_entry bytecode stack pc sp bc_ptr st_ptr =
  Debug.print_arr string_of_int stack ~notation:(Some "stack") ;
  if !Config.jit_flag = `Off then ()
  else if Trace_prof.over_threshold pc then
    begin
      match Trace_prof.find_opt pc with
      | Some _ -> ()
      | None ->
         let ic = file_open () in
         try
           let prog =
             ic |> Lexing.from_channel |> Util.virtualize
             |> Jit_annot.annotate `Meta_tracing
           in
           close_in ic;
           let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
           match prog |> jit_tracing env with
           | Ok name -> Trace_prof.register (pc, name);
           | Error e -> raise e
         with e -> close_in ic; raise e
    end
  else Trace_prof.count_up pc

let jit_method_call bytecode stack pc sp bc_ptr st_ptr =
    match Method_prof.find_opt pc with
  | Some name ->
     let s = Unix.gettimeofday () in
     let r = exec_dyn_arg2 ~name:name ~arg1:st_ptr ~arg2:sp in
     let e = Unix.gettimeofday () in
     Printf.printf "[mj] elapced time: %fms\n" ((e -. s) *. 1000.); flush stdout;
     r
  | None ->
     let ic = file_open () in
     try
       let p =
         ic |> Lexing.from_channel |> Util.virtualize
         |> Jit_annot.annotate `Meta_method
       in
       close_in ic;
       let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
       match p |> jit_method env with
       | Ok name ->
          Printf.printf "[mj] compiled %s at pc: %d\n" name pc;
          Method_prof.register (pc, name);
          let s = Unix.gettimeofday () in
          let r = exec_dyn_arg2 ~name:name ~arg1:st_ptr ~arg2:sp in
          let e = Unix.gettimeofday () in
          Printf.printf "[mj] elapced time: %fms\n" ((e -. s) *. 1000.); flush stdout;
          r
       | Error e -> raise e
     with e -> close_in ic; raise e

let callbacks () =
  Callback.register "jit_tracing_entry" jit_tracing_entry;
  Callback.register "jit_exec" jit_exec;
  Callback.register "jit_method_call" jit_method_call;
