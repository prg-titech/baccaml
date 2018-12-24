open OUnit2
open MinCaml
open Bc_jit

let test_1 _ =
  let input =
    Asm.Let (("x.2", Type.Int), Asm.Mov ("x.1"),
             Asm.Let (("y.4", Type.Int), Asm.Add ("x.2", C (10)),
                      Asm.Ans (Asm.Mov ("y.4")))) in
  let result = Jit_elim.elim input in
  let expected =
    Asm.Let (("y.4", Type.Int), Asm.Add ("x.1", C (10)),
             Asm.Ans (Asm.Mov ("y.4"))) in
  assert_equal ~printer:Asm.show expected result

let test_2 _ =
  let input =
    Asm.Let (("x.2", Type.Int), Asm.Mov ("x.1"),
             Asm.Let (("y.4", Type.Int), Asm.Add ("x.2", C (10)),
                      Asm.Let (("z.6", Type.Int), Asm.Add ("y.4", V ("x.2")),
                               Asm.Let (("z.8", Type.Int), Mov ("z.6"),
                                       Asm.Ans (Asm.Mov ("z.8"))))))
  in
  let result = Jit_elim.elim input in
  let expected =
    Asm.Let (("y.4", Type.Int), Asm.Add ("x.1", C (10)),
             Asm.Let (("z.6", Type.Int), Asm.Add ("y.4", V ("x.1")),
                     Asm.Ans (Asm.Mov ("z.6")))) in
  assert_equal ~printer:Asm.show expected result


let suite =
  "jit_elim_test_suites" >:::
  ["test_1" >:: test_1;
   "test_2" >:: test_2]

let _ =
  run_test_tt_main suite
