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
        reg.(42) <- Red (100);
        let res = jitcompile (Prog ([], [], instr)) instr reg mem in
        assert_equal (value_of reg.(81)) 2;
        assert_equal res (Let (("Ti22.83", Type.Int), Add ("a.42", C (1)),
                               Ans (Mov ("a.42"))))
      end;
      "test 2" >::
      begin fun () ->
        let instr =
          Let (("Ti25.77", Type.Int), Add ("pc.41", C (1)),
               Let (("Ti27.39", Type.Int), Sub ("a.42", C (1)),
                    Ans (Mov ("a.42"))))
        in
        let reg = Array.make 100 (Red (0)) in
        let mem = Array.make 100 (Red (0)) in
        reg.(41) <- Green (1);
        reg.(42) <- Red (100);
        let res = jitcompile (Prog ([], [], instr)) instr reg mem in
        assert_equal (value_of reg.(77)) 2;
        assert_equal res (Let (("Ti27.39", Type.Int), Sub ("a.42", C (1)),
                               Ans (Mov ("a.42"))))
      end
    ]
  end
