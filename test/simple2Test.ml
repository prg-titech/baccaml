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
  let reg = Array.create 10000 (Red (0)) in
  let mem = Array.create 10000 (Red (0)) in
  let bytecode = [|0; 11; 4; 7; 0; 0; 20; 1; 1; 20|] in
  for i = 0 to (Array.length bytecode - 1) do
    mem.(0 + i * 4) <- Green (i)
  done;
  let res = method_jit prog body reg mem method_jit_args in
  print_string (EmitVirtual.to_string_t res);
  ()

(*
compiled code:
  Let ((instr.97, Int), Ld (code.58, "pc.59", 4),
  Ans (IfEq (instr.97, 0,
  Let ((Ti38.118, Int), Add (pc.59, "Ti37.117"),
  Let ((Ti40.120, Int), Add (a.60, "Ti39.119"),
  Ans (CallDir (Id.L ("interp.57"), [code.58; Ti38.118; Ti40.120], [])))),
  Ans (IfEq (instr.97, 1,
  Let ((Ti43.114, Int), Add (pc.59, "Ti42.113"),
  Let ((Ti45.116, Int), Sub (a.60, "Ti44.115"),
  Ans (CallDir (Id.L ("interp.57"), [code.58; Ti43.114; Ti45.116], [])))),
  Ans (IfEq (instr.97, 10,
  Let ((Ti48.111, Int), Add (pc.59, "Ti47.110"),
  Let ((t.112, Int), Ld (code.58, "Ti48.111", 4),
  Ans (CallDir (Id.L ("interp.57"), [code.58; t.112; a.60], [])))),
  Ans (IfEq (instr.97, 11,
  Ans (IfLE (a.60, 0,
  Let ((Ti52.108, Int), Add (pc.59, "Ti51.107"),
  Let ((t2.109, Int), Ld (code.58, "Ti52.108", 4),
  Ans (CallDir (Id.L ("interp.57"), [code.58; t2.109; a.60], [])))),
  Let ((Ti54.105, Int), Add (pc.59, "Ti53.104"),
  Let ((t1.106, Int), Ld (code.58, "Ti54.105", 4),
  Ans (CallDir (Id.L ("interp.57"), [code.58; t1.106; a.60], [])))))),
  Ans (IfEq (instr.97, 20,
  Ans (Mov (a.60)),
  Ans (Set (-1)))))))))))))
*)
