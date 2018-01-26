open Asm
open Core
open Method_jit
open Jit_config
open Mincaml_util
open Test_util

let prog = virtualize (Lexing.from_channel (In_channel.create (dir ^ "mtj-call.ml")))

let Prog (_, fundefs, main) = prog

let fundef = match List.hd fundefs with
    Some v -> v
  | None -> failwith "List.hd is failed."

let jit_args = {
  trace_name = "test_trace.1000";
  reds = ["stack.158"];
  greens = [];
  loop_header = 0;
  loop_pc_place = 0
}

let _ =
  let { body; } = fundef in
  let reg = Array.create 10000 (Red 0) in
  let mem = Array.create 10000 (Red 0) in
  reg.(156) <- Green (0);
  reg.(157) <- Green (0);
  reg.(158) <- Red (100);
  reg.(159) <- Green (0);
  let res = method_jit prog body reg mem jit_args in
  print_string (Emit_virtual.to_string_t res);
  ()
