open Asm
open Core
open JitConfig
open Jitinterop
open OUnit

let print_string_list l =
  "[ " ^ (String.concat ~sep:"; " l) ^ " ]"

let print_string_int l =
  "[ " ^ (String.concat ~sep:"; " (List.map ~f:string_of_int l)) ^ " ]"

let prog1 =
  let instr1 =
    Let (
      ("Ti1.1", Type.Int), Mov ("a.100"),
      Let (
        ("a.2.101", Type.Int), Add ("a.100", C (200)),
        Let (
          ("b.3.102", Type.Int), Ld ("Ti4.4", C (200), 4),
          Ans (Mov ("b.3.102"))
        )
      )
    ) in Prog ([], [], instr1)

let _ = run_test_tt_main begin
    "jit_interp_test" >::: [
      "find_red" >:: begin
        fun () ->
          let Prog (_, _, instr) = prog1 in
          let reds = ["a"; "b"] in
          assert_equal
            ~printer:print_string_list
            (find_red_in_body instr reds)
            (["a.100"; "a.2.101"; "b.3.102"])
      end;
      "convert" >:: begin
        fun () ->
          let reds = ["a"; "b"] in
          let reg = Array.create 1000 0 in
          reg.(100) <- 100;
          reg.(102) <- 200;
          let expected = Array.create (Array.length reg) (Green (0)) in
          expected.(100) <- Red (100);
          expected.(101) <- Red (0);
          expected.(102) <- Red (200);
          assert_equal
            (convert prog1 reg reds)
            expected
      end
    ]
  end
