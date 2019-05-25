open Utils
open Std
open MinCaml
open Bc_jit

module E = Bc_jit.Jit_emit_base

exception Error of string

let file_name = ref None

let size = 10000

(* TODO: specify extenally *)
let greens = ["pc"; "bytecode"]

let bc_tmp_addr = 0

let st_tmp_addr = 100

let pc_method_annot_inst = 15

let print_arr ?notation:(nt = None) f arr =
  let str = Array.string_of_array f arr in
  match nt with
  | Some s -> Printf.eprintf "%s %s\n" s str
  | None -> Printf.eprintf "%s\n" str

let file_open () =
  match !file_name with
  | Some name -> open_in name
  | None -> raise @@ Error "argument is not specified."

let get_ir_addr args name =
  args
  |> List.find (fun a -> String.get_name a = name)
  |> String.get_extension |> int_of_string

let counter = ref 0

let gen_trace_name : [< `Meta_tracing | `Meta_method] -> string =
 fun typ ->
  let mark = match typ with `Meta_tracing -> "tj" | `Meta_method -> "mj" in
  let name = "trace" ^ mark ^ string_of_int !counter in
  incr counter ; Id.genid name

let get_dylib_name : string -> string = fun name -> "lib" ^ name ^ ".dylib"

let get_red_args args =
  args |> List.filter (fun a -> not (List.mem (String.get_name a) greens))

let make_reg prog args sp =
  let reg = Array.make size (Jit_util.Red 0) in
  let Asm.{args; body= t} = Jit_util.find_fundef' prog "interp" in
  Asm.fv t @ args
  |> List.iteri (fun i a ->
         if List.mem (String.get_name a) greens then reg.(i) <- Green 0
         else reg.(i) <- Red 0 ) ;
  reg

let make_mem ~bc_addr ~st_addr bytecode stack =
  let mem = Array.make size (Jit_util.Green 0) in
  bytecode |> Array.iteri (fun i a -> mem.(bc_addr + (4 * i)) <- Jit_util.Green a) ;
  stack |> Array.iteri (fun i a -> mem.(st_addr + (4 * i)) <- Jit_util.Red a) ;
  mem

let emit_dyn : 'a -> Asm.fundef list -> unit =
 fun env traces ->
  traces |> List.map Simm.h |> List.map RegAlloc.h |> E.emit_dynamic env

let compile_dyn : string -> 'a =
 fun name ->
  let trace_file_name = name ^ ".s" in
  let dylib = get_dylib_name name in
  let cmd =
    Printf.sprintf
      "gcc '-m32' '-dynamiclib' '-O2' '-Wl,-undefined' '-Wl,dynamic_lookup' -o %s %s"
      dylib trace_file_name
  in
  Log.debug cmd ;
  match Unix.system cmd with
  | Unix.WEXITED (i) when i = 0 -> Try.Success (name)
  | _ -> Try.Failure (Exit)

type env_jit =
  { bytecode: int array
  ; stack: int array
  ; pc: int
  ; sp: int
  ; bc_ptr: int
  ; st_ptr: int }

let jit_method {bytecode; stack; pc; sp; bc_ptr; st_ptr} prog =
  let prog = Jit_annot.annotate `Meta_method prog in
  let Asm.{args; body} = Jit_util.find_fundef' prog "interp" in
  let reg = make_reg prog args sp in
  let mem =
    make_mem ~bc_addr:bc_tmp_addr ~st_addr:st_tmp_addr bytecode stack
  in
  let pc_method_entry =
    bytecode |> Array.to_list
    |> List.mapi (fun i a -> (i, a))
    |> List.find (fun (i, a) -> a = pc_method_annot_inst)
    |> fst
  in
  Printf.eprintf "pc_method_entry: %d\n" pc_method_entry ;
  let pc_ir_addr = get_ir_addr args "pc" in
  let sp_ir_addr = get_ir_addr args "sp" in
  let bc_ir_addr = get_ir_addr args "bytecode" in
  let st_ir_addr = get_ir_addr args "stack" in
  reg.(pc_ir_addr) <- Green pc_method_entry ;
  reg.(sp_ir_addr) <- Red sp ;
  reg.(bc_ir_addr) <- Green bc_tmp_addr ;
  reg.(st_ir_addr) <- Red st_tmp_addr ;
  let module JM = Jit_method in
  let trace_name = gen_trace_name `Meta_method in
  Printf.eprintf "trace_name %s\n" trace_name ;
  let env =
    { JM.trace_name
    ; JM.red_args= get_red_args args
    ; JM.index_pc= 3
    ; JM.merge_pc= pc_method_entry }
  in
  let traces = JM.run prog reg mem env in
  let emit_env = {E.out= trace_name; E.jit_typ= `Meta_method; E.prog} in
  emit_dyn emit_env traces ; compile_dyn trace_name

let jit_tracing {bytecode; stack; pc; sp; bc_ptr; st_ptr} prog =
  let prog = Jit_annot.annotate `Meta_tracing prog in
  let Asm.{args; body} = Jit_util.find_fundef' prog "interp" in
  let reg = make_reg prog args sp in
  let mem =
    make_mem ~bc_addr:bc_tmp_addr ~st_addr:st_tmp_addr bytecode stack
  in
  let pc_ir_addr = get_ir_addr args "pc" in
  let sp_ir_addr = get_ir_addr args "sp" in
  let bc_ir_addr = get_ir_addr args "bytecode" in
  let st_ir_addr = get_ir_addr args "stack" in
  reg.(pc_ir_addr) <- Green pc ;
  reg.(sp_ir_addr) <- Red sp ;
  reg.(bc_ir_addr) <- Green bc_tmp_addr ;
  reg.(st_ir_addr) <- Red st_tmp_addr ;
  let module JT = Jit_tracing in
  let trace_name = gen_trace_name `Meta_tracing in
  Printf.eprintf "trace_name %s\n" trace_name ;
  let env =
    { JT.index_pc= 3
    ; JT.merge_pc= pc
    ; JT.trace_name
    ; JT.red_args=
        args |> List.filter (fun a -> not (List.mem (String.get_name a) greens))
    ; JT.bytecode_ptr= bc_ptr
    ; JT.stack_ptr= st_ptr }
  in
  let trace = JT.run prog reg mem env in
  Log.debug (Emit_virtual.string_of_fundef trace);
  let emit_env = {E.out= trace_name; E.jit_typ= `Meta_tracing; E.prog} in
  emit_dyn emit_env [trace] ;
  compile_dyn trace_name


module Trace : sig
  val get_count_hash : unit -> (int, int) Hashtbl.t
  val count_up : int -> unit
  val not_compiled : int -> bool
  val has_compiled : int -> unit
  val over_threshold : int -> bool
end = struct
  let threshold = 2

  let count_hash = Hashtbl.create 100

  let compiled_hash = Hashtbl.create 100

  let get_count_hash () = count_hash

  let count_up pc =
    match Hashtbl.find_opt count_hash pc with
    | Some v ->
       Hashtbl.replace count_hash pc (v + 1)
    | None ->
       Hashtbl.add count_hash pc 1

  let not_compiled pc =
    match Hashtbl.find_opt compiled_hash pc with
      Some _ -> false | None -> true

  let has_compiled pc =
    match Hashtbl.find_opt compiled_hash pc with
    | Some v -> Hashtbl.replace compiled_hash pc true
    | None -> Hashtbl.add compiled_hash pc true

  let over_threshold pc =
    match Hashtbl.find_opt count_hash pc with
    | Some count ->
       if count > threshold then begin
           true
       end else
         false
    | None ->
       false
end

let exec_dyn ~name ~arg1 ~arg2 =
  Dynload_stub.call_arg2
    ~lib:("./" ^ get_dylib_name name) ~func:(String.split_on_char '.' name |> List.hd)
    ~arg1:arg1 ~arg2:arg2

let jit_entry bytecode stack pc sp bc_ptr st_ptr =
  print_arr string_of_int bytecode ~notation:(Some "bytecode") ;
  print_arr string_of_int stack ~notation:(Some "stack") ;
  Log.debug (Printf.sprintf "pc %d, sp %d, bc_ptr %d, st_ptr %d" pc sp bc_ptr st_ptr);
  if Trace.over_threshold pc && (Trace.not_compiled pc) then
    let prog =
      let ic = file_open () in
      try
        let module A = Jit_annot in
        let v = ic |> Lexing.from_channel |> Util.virtualize in
        close_in ic ; v
      with e -> close_in ic ; raise e
    in
    let module E = Jit_emit_base in
    let env = {bytecode; stack; pc; sp; bc_ptr; st_ptr} in
    begin match prog |> jit_tracing env with
    | Try.Success name ->
       (* let r = exec_dyn ~name:name ~arg1:st_ptr ~arg2:sp in
        * print_int r; print_newline () *)
       ()
    | Try.Failure e -> raise e
    end;
    Trace.has_compiled pc
  else
    Trace.count_up pc

let () =
  if Array.length Sys.argv < 2 then raise @@ Error "please specify your file."
  else file_name := Some Sys.argv.(1) ;
  Log.log_level := `Debug ;
  Callback.register "jit_entry" jit_entry
