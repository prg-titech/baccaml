open Utils
open Std
open MinCaml
open Asm
open Bc_jit
open Jit_util

exception Error of string

let file_name = ref None

let size = 10000

(* TODO: specify extenally *)
let greens = ["pc"; "bytecode"]

let bc_tmp_addr = 0

let st_tmp_addr = 100

let file_open () =
  match !file_name with
  | Some name -> open_in name
  | None -> raise @@ Error "argument is not specified."

let print_arr ?notation:(nt = None) f arr =
  let str = Array.string_of_array f arr in
  match nt with
  | Some s -> Printf.eprintf "%s %s\n" s str
  | None -> Printf.eprintf "%s\n" str

let get_ir_addr args name =
  args
  |> List.find (fun a -> String.get_name a = name)
  |> String.get_extension |> int_of_string

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

let jit_entry bytecode stack pc sp bc_ptr st_ptr =
  print_arr string_of_int bytecode ~notation:(Some "bytecode") ;
  print_arr string_of_int stack ~notation:(Some "stack") ;
  Printf.eprintf "pc %d, sp %d, bc_ptr %d, st_ptr %d\n" pc sp bc_ptr st_ptr ;
  let prog =
    let ic = file_open () in
    try
      let v = ic |> Lexing.from_channel |> Util.virtualize in
      close_in ic ; v
    with e -> close_in ic ; raise e
  in
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
  let env =
    Jit_tracing.
      { index_pc= 3
      ; merge_pc= pc
      ; trace_name= "test_trace"
      ; red_args= args |> List.filter (fun a -> not (List.mem (String.get_name a) greens))
      ; bytecode_ptr = bc_ptr
      ; stack_ptr = st_ptr
      }
  in
  let trace = Jit_tracing.run prog reg mem env in
  print_endline (Emit_virtual.string_of_fundef trace) ;
  ()

let () =
  file_name := Some Sys.argv.(1) ;
  Log.log_level := `Debug ;
  Callback.register "jit_entry" jit_entry
