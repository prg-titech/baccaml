open MinCaml
open Asm

open Jit
module C = Jit_constfold

open OUnit2

let test_folding_simple _ =
  let testee =
    let open Type in
    Let (("x.1", Int), Set (1),
         Let (("x.2", Int), Mov ("x.1"),
              Let (("y.3", Int), Add ("x.2", C (2)),
                   Ans (IfEq ("y.3", C (3),
                              Ans (Mov "x.2"),
                              Ans (Set (1)))))))
  and expected =
    Let (("x.1", Int), Set (1),
         Let (("y.3", Int), Add ("x.1", C (2)),
              Ans (IfEq ("y.3", C (3),
                         Ans (Mov "x.1"),
                         Ans (Set (1))))))
  in assert_equal
    ~msg:"test_folding_simple"
    ~printer:Asm.show
    expected
    (C.folding testee)

let test_folding _ =
  let testee =
    let open Type in
    Let (("stack.1", Int), Set (100),
         Let (("stack.2", Int), Mov ("stack.1"),
              Let (("stack.3", Int), Mov ("stack.2"),
                   Let (("x.4", Int), Set (2),
                        Ans (IfEq ("x.4", C (10),
                                   Ans (Set (1000)),
                                   Ans (Mov ("stack.3"))))
                       ))))
  and expected =
    Let (("stack.1", Int), Set (100),
         Let (("x.4", Int), Set (2),
              Ans (IfEq ("x.4", C (10),
                         Ans (Set (1000)),
                         Ans (Mov ("stack.1"))))
             ))
  in assert_equal
    ~msg:"test_folding"
    ~printer:Asm.show
    expected (C.iter ~n:5 testee)

let suite =
  "TestJitConstfold" >::: [
    "test_folding_simple" >:: test_folding_simple;
    "test_folding" >:: test_folding
  ]

let _ =
  run_test_tt_main suite
