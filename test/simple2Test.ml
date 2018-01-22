open Asm
open Core
open Type
open Core
open OUnit
open MethodJit
open JitConfig
open MincamlUtil
open TestUtil

let prog =
  In_channel.create (dir ^ "simple2.ml")
  |> Lexing.from_channel
  |> virtualize

let Prog (_, fundefs, main) = prog

let fundef = match List.hd fundefs with
    Some v -> v
  | None -> failwith "List.hd is failed."

let method_jit_args = {
  method_name = "test_method.1000";
  reds = ["a.69"];
  method_start = 0;
  method_end = 12;
  pc_place = 1
}

let _ =
  let { body; } = fundef in
  let reg = Array.create 10000 (Red (0)) in
  let mem = Array.create 10000 (Red (0)) in
  let bytecode = [|0; 11; 4; 8; 0; 0; 10; 12; 1; 1; 10; 12; 20|] in
  for i = 0 to (Array.length bytecode - 1) do
    mem.(0 + i * 4) <- Green (bytecode.(i))
  done;
  reg.(67) <- Green (0);
  reg.(68) <- Green (0);
  let res = exec_method_jit prog body reg mem method_jit_args in
  print_string (EmitVirtual.to_string_fundef res);
  ()

(*
compiled code:
  { name = "test_method.1000"; args = [a.69]; fargs = []; body =
  Let ((Ti49.138, Int), Add (a.69, 1),
  Let ((a.69, Int), Mov (Ti49.138),
  Ans (IfLE (a.69, 0,
  Let ((Ti54.134, Int), Sub (a.69, 1),
  Let ((a.69, Int), Mov (Ti54.134),
  Let ((Ti54.134, Int), Sub (a.69, 1),
  Let ((a.69, Int), Mov (Ti54.134),
  Ans (Mov (a.69)))))),
  Let ((Ti49.138, Int), Add (a.69, 1),
  Let ((a.69, Int), Mov (Ti49.138),
  Let ((Ti49.138, Int), Add (a.69, 1),
  Let ((a.69, Int), Mov (Ti49.138),
  Ans (Mov (a.69)))))))))); ret = Int }
*)
