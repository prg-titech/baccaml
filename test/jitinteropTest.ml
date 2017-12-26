open Asm
open Core
open Jitinterop
open OUnit

let _ = run_test_tt_main begin
    "jit_interp_test" >::: [
      "find_red" >:: begin
        fun () ->
          let instr =
            Let (
              ("Ti1.1", Type.Int), Mov ("a.100"),
              Let (
                ("a.2.101", Type.Int), Add ("a.100", C (200)),
                Let (
                  ("b.3.102", Type.Int), Ld ("Ti4.4", C (200), 4),
                  Ans (Mov ("b.3.102"))
                )
              )
            )
          in
          let printer l = "[ " ^ (String.concat ~sep:"; " l) ^ " ]" in
          let reds = ["a"; "b"] in
          assert_equal
            ~printer:printer
            (find_red_in_body instr reds)
            (["a.100"; "a.2.101"; "b.3.102"])
      end
    ]
  end
