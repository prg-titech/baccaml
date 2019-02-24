open Utils
open Std
open MinCaml
open Asm
open Bc_jit
open Jit_util

module E = Jit_emit_base

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

let gen_trace_name : unit -> string = fun () ->
  let name = "trace" ^ string_of_int !counter in
  incr counter ; Id.genid name

let get_red_args args =
  args |> List.filter (fun a -> not (List.mem (String.get_name a) greens))

let make_reg prog args sp =
  let reg = Array.make size (Red 0) in
  let {args; body= t} = find_fundef' prog "interp" in
  fv t @ args
  |> List.iteri (fun i a ->
         if List.mem (String.get_name a) greens then reg.(i) <- Green 0
         else reg.(i) <- Red 0 ) ;
  reg

let make_mem ~bc_addr ~st_addr bytecode stack =
  let mem = Array.make size (Green 0) in
  bytecode |> Array.iteri (fun i a -> mem.(bc_addr + (4 * i)) <- Green a) ;
  stack |> Array.iteri (fun i a -> mem.(st_addr + (4 * i)) <- Red a) ;
  mem

let emit_dyn : 'a -> fundef list -> unit = fun env traces ->
  traces |> List.map Simm.h |> List.map RegAlloc.h |> E.emit_dynamic env

let compile_dyn : string -> unit = fun name ->
  let trace_file_name = name ^ ".s" in
  let dylib = Printf.sprintf "lib%s.dylib" name in
  let cmd =
    Printf.sprintf "gcc '-m32' '-dynamiclib' '-Wl,-undefined' '-Wl,dynamic_lookup' -o %s %s"
      dylib trace_file_name
  in
  Log.debug (cmd);
  match Unix.system cmd with
  | Unix.WEXITED _ -> ()
  | Unix.WSIGNALED _ -> ()
  | Unix.WSTOPPED _ -> ()

type env_jit =
  {bytecode: int array; stack: int array; pc: int; sp: int; bc_ptr: int; st_ptr: int}

let jit_method {bytecode; stack; pc; sp; bc_ptr; st_ptr} prog =
  let prog = Jit_annot.annotate `Meta_method prog in
  let {args; body} = find_fundef' prog "interp" in
  let reg = make_reg prog args sp in
  let mem = make_mem ~bc_addr:bc_tmp_addr ~st_addr:st_tmp_addr bytecode stack in
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
  let trace_name = gen_trace_name () in
  Printf.eprintf "trace_name %s\n" trace_name;
  let env =
    { JM.trace_name= trace_name
    ; JM.red_args= get_red_args args
    ; JM.index_pc= 3
    ; JM.merge_pc= pc_method_entry }
  in
  let traces = JM.run prog reg mem env in
  let emit_env = {E.out= trace_name; E.jit_typ= `Meta_method; E.prog= prog} in
  emit_dyn emit_env traces;
  compile_dyn trace_name

let jit_tracing {bytecode; stack; pc; sp; bc_ptr; st_ptr} prog =
  let prog = Jit_annot.annotate `Meta_tracing prog in
  let {args; body} = find_fundef' prog "interp" in
  let reg = make_reg prog args sp in
  let mem = make_mem ~bc_addr:bc_tmp_addr ~st_addr:st_tmp_addr bytecode stack in
  let pc_ir_addr = get_ir_addr args "pc" in
  let sp_ir_addr = get_ir_addr args "sp" in
  let bc_ir_addr = get_ir_addr args "bytecode" in
  let st_ir_addr = get_ir_addr args "stack" in
  reg.(pc_ir_addr) <- Green pc ;
  reg.(sp_ir_addr) <- Red sp ;
  reg.(bc_ir_addr) <- Green bc_tmp_addr ;
  reg.(st_ir_addr) <- Red st_tmp_addr ;
  let module JT = Jit_tracing in
  let trace_name = gen_trace_name () in
  Printf.eprintf "trace_name %s\n" trace_name;
  let env =
    { JT.index_pc= 3
    ; JT.merge_pc= pc
    ; JT.trace_name= trace_name
    ; JT.red_args=
        args |> List.filter (fun a -> not (List.mem (String.get_name a) greens))
    ; JT.bytecode_ptr= bc_ptr
    ; JT.stack_ptr= st_ptr }
  in
  let trace = JT.run prog reg mem env in
  let emit_env = {E.out= trace_name; E.jit_typ= `Meta_tracing; E.prog= prog} in
  emit_dyn emit_env [trace];
  compile_dyn trace_name

let jit_entry bytecode stack pc sp bc_ptr st_ptr =
  print_arr string_of_int bytecode ~notation:(Some "bytecode") ;
  print_arr string_of_int stack ~notation:(Some "stack") ;
  Printf.eprintf "pc %d, sp %d, bc_ptr %d, st_ptr %d\n" pc sp bc_ptr st_ptr ;
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
  prog |> jit_tracing env;
  prog |> jit_method env;
  ()

let () =
  if Array.length Sys.argv < 2 then raise @@ Error "please specify your file."
  else file_name := Some Sys.argv.(1) ;
  Log.log_level := `Debug ;
  Callback.register "jit_entry" jit_entry
