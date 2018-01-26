open OUnit
open Asm
open Type
open Util

let _ = run_test_tt_main begin
            "interp_tests" >::: [
              "test 1" >::
              begin fun () ->
              let instr = Let (("Ti20.81", Type.Int), Add ("pc.41", C (1)),
                               Let (("Ti22.83", Type.Int), Add ("a.42", C (1)),
                                    Ans (Mov ("Ti22.83"))))
              in
              let p = Prog ([], [], instr) in
              let res = Interp.f p in
              assert_equal res 1
              end
            ]
          end
