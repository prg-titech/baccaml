open Asm
open Core
open OUnit
open MethodJit
open JitConfig
open Type

let _ = run_test_tt_main begin
    "method_jit_test" >::: [
      "test_1" >::
      begin fun () ->
        let instr =
          Let (("Ti1.1", Type.Int), Set (1),
               Let (("Ti2.2", Type.Int), Set (2),
                    Ans (IfEq ("Ti2.2", C (2),
                               Let (("Ti3.3", Type.Int), Add ("Ti1.1", V ("Ti2.2")),
                                    Ans (Mov ("Ti3.3"))),
                               Ans (Mov ("Ti1.1"))))))
        in
        let prog = Prog ([], [], instr) in
        let reg = Array.create 1000 (Red (0)) in
        let mem = Array.create 1000 (Red (0)) in
        reg.(1) <- Green (0); reg.(2) <- Green (0);
        assert_equal
          ~printer:EmitVirtual.to_string_t
          (method_jit prog instr reg mem dummy_jit_args)
          (Ans (IfEq ("Ti2.2", C (2),
                      Ans (Mov ("Ti3.3")),
                      Ans (Mov ("Ti1.1")))));
        ()
      end;
    ]
  end
