open OUnit
open Asm
open Jit

let _ = run_test_tt_main begin
    "jit_tests" >::: [
      "test 1" >::
      begin fun () ->
        let instr =
          Let (("Ti20.81", Type.Int), Add ("pc.41", C (1)),
               Let (("Ti22.83", Type.Int), Add ("a.42", C (1)),
                    Ans (Mov ("a.42"))))
        in
        let reg = Array.make 100 (Red (0)) in
        let mem = Array.make 100 (Red (0)) in
        reg.(41) <- Green (1);
        let res = jitcompile instr reg mem in
        assert_equal (value_of reg.(81)) 2;
        assert_equal res (Let (("Ti22.83", Type.Int), Add ("a.42", C (1)),
                               Ans (Mov ("a.42"))))
      end
    ]
  end
