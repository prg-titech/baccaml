open Utils
open Std
open MinCaml
open Asm
open Bc_jit
open Jit_util

(* For test *)
let dummy_fun x =
  print_string "apply dummy_fun to " ;
  print_int x ;
  print_newline () ;
  print_string "executed file is " ;
  print_endline Sys.argv.(0) ;
  x + 1

let size = 100000

(* TODO: specify extenally *)
let greens = ["pc"; "bytecode"]

let bc_addr = 0

let st_addr = 100

let make_reg prog sp =
  let reg = Array.make size (Red 0) in
  let {args; body= t} = find_fundef' prog "interp" in
  fv t
  |> List.iteri (fun i a ->
         if List.mem (String.get_name a) greens then reg.(i) <- Green 0
         else reg.(i) <- Red 0 ) ;
  reg

let make_mem bytecode stack =
  let mem = Array.make size (Green 0) in
  bytecode |> Array.iteri (fun i a -> mem.(bc_addr + i) <- Green a) ;
  stack |> Array.iteri (fun i a -> mem.(st_addr + i) <- Red a) ;
  mem

let jit_entry bytecode stack pc sp bc_ptr st_ptr =
  Array.print_array print_int bytecode ;
  print_newline () ;
  Array.print_array print_int stack ;
  print_newline () ;
  Printf.eprintf "pc %d, sp %d, bc_ptr %d, st_ptr %d\n" pc sp bc_ptr st_ptr;
  let prog =
    let ic = open_in "./test_interp.mcml" in
    try
      let v = ic |> Lexing.from_channel |> Util.virtualize in
      close_in ic ; v
    with e -> close_in ic ; raise e
  in
  (* let reg = make_reg prog sp in
   * let mem = make_mem bytecode stack in
   * Jit_tracing.(
   *   let env = {
   *       index_pc = 2;
   *       merge_pc = pc;
   *       trace_name = "test_trace";
   *       red_args = ["stack"; "sp"];
   *     } in
   *   let trace = Jit_tracing.run prog reg mem env in
   *   print_endline (Emit_virtual.string_of_fundef trace)
   * ); *)
  ()

let () =
  Callback.register "jit_entry" jit_entry ;
  Callback.register "dummy_fun" dummy_fun
