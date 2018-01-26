open Asm
open OUnit
open Type
open Tracing_jit
open Jit_config


let jit_args = {
  trace_name = "jittest";
  reds = ["var.42"];
  greens = [];
  loop_header = 0;
  loop_pc_place = 0
}

let _ = run_test_tt_main begin
    "jit_tests" >::: [
      "test 1" >::
      begin fun () ->
        let instr =
          Let (("Ti20.81", Type.Int), Add ("pc.41", C (1)),
               Let (("Ti22.83", Type.Int), Add ("a.42", C (1)),
                    Ans (Mov ("a.42"))))
        in
        let p = Prog ([], [], instr) in
        let reg = Array.make 100 (Red (0)) in
        let mem = Array.make 100 (Red (0)) in
        reg.(41) <- Green (1);
        reg.(42) <- Red (100);
        let res = tracing_jit p instr reg mem jit_args in
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
        let p = Prog ([], [], instr) in
        let reg = Array.make 100 (Red (0)) in
        let mem = Array.make 100 (Red (0)) in
        reg.(41) <- Green (1);
        reg.(42) <- Red (100);
        let res = tracing_jit p instr reg mem jit_args in
        assert_equal (value_of reg.(77)) 2;
        assert_equal res (Let (("Ti27.39", Type.Int), Sub ("a.42", C (1)),
                               Ans (Mov ("a.42"))))
      end;
      (*"test 3" >::
      begin fun () ->
        let instr =
          Let (("Ti1.13", Int), Set (30),
               Let (("Ti2.14", Int), Set (10),
                    Let (("Ti4.16", Int), CallDir (Id.L ("f.9"), ["Ti1.13"; "Ti2.14"], []),
                         Let (("Ti.17", Int), Sub ("Ti5.27", C (1)),
                              Ans (Mov ("Ti4.16"))))))
        in
        let fundef =
          { name = Id.L "f.9"
          ; args = ["n.10"; "a.11"]
          ; fargs = []
          ; body = Let (("Ti5.27", Int), Add ("n.10", V "a.11"), Ans (Mov ("Ti5.27")))
          ; ret = Int
          }
        in
        let prog = Prog ([], [fundef], instr) in
        let reg = Array.make 100 (Red (0)) in
        let mem = Array.make 100 (Red (0)) in
        reg.(13) <- Green (0);
        reg.(14) <- Green (0);
        let res = tracing_jit prog instr reg mem jit_args in
        assert_equal (reg.(13)) (Green (30));
        assert_equal (reg.(14)) (Green (10));
        assert_equal res (Let (("n.10", Int), Mov ("Ti1.13"),
                               Let (("a.11", Int), Mov ("Ti2.14"),
                                    Let (("Ti5.27", Int), Add ("n.10", V "a.11"),
                                         Let (("Ti4.16", Int),  Mov ("Ti5.27"),
                                              Let (("Ti.17", Int), Sub ("Ti5.27", C (1)),
                                                   Ans (Mov ("Ti4.16"))))))));
        end;*)
      "test 4" >::
      begin fun () ->
        let instr =
          Ans(IfEq("Ti1.1", V ("Ti2.2"),
                   Let (("Ti3.3", Int), Mov ("Ti4.4"), Ans (CallDir (Id.L ("min_caml_print_int"), ["Ti4.4"], []))),
                   Ans (Mov ("Ti5.5"))
                  ))
        in
        let prog = Prog ([], [], instr) in
        let reg = Array.make 100 (Red (0)) in
        let mem = Array.make 100 (Red (0)) in
        reg.(1) <- Green (1);
        reg.(2) <- Green (2);
        let res = tracing_jit prog instr reg mem jit_args in
        assert_equal res (Ans (Mov ("Ti5.5")))
      end
    ]
  end
