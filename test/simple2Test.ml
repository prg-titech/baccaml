open Asm
open Core
open Type
open Core
open OUnit
open MethodJit
open JitConfig
open MincamlUtil
open TestUtil

let prog = virtualize (Lexing.from_channel (In_channel.create (dir ^ "simple2.ml")))

let Prog (_, fundefs, main) = prog

let fundef = match List.hd fundefs with
    Some v -> v
  | None -> failwith "List.hd is failed."

let method_jit_args = {
  method_name = "test_method.1000";
  reds = ["a.60"];
  method_start = 0;
  method_end = 9;
  pc_place = 2
}

let _ =
  let { body; } = fundef in
  let reg = Array.create 10000 (Red (-1)) in
  let mem = Array.create 10000 (Red (-1)) in
  reg.(58) <- Green (0);
  reg.(59) <- Green (0);
  reg.(60) <- Red (0);
  let bytecode = [|0; 11; 4; 7; 0; 0; 20; 1; 1; 20|] in
  for i = 0 to (Array.length bytecode - 1) do
    mem.(0 + i * 4) <- Green (i)
  done;
  let res = method_jit prog body reg mem method_jit_args in
  print_string (EmitVirtual.to_string_t res);
  ()
