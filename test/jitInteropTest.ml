open Asm
open Core
open JitConfig
open JitInterop
open OUnit

let print_string_list l =
  "[ " ^ (String.concat ~sep:"; " l) ^ " ]"

let print_string_int l =
  "[ " ^ (String.concat ~sep:"; " (List.map ~f:string_of_int l)) ^ " ]"

let string_of_value = function
  Green n -> "Green (" ^ (string_of_int n) ^ ")"
  | Red n -> "Red (" ^ (string_of_int n) ^ ")"

let string_of_value_array arr =
  let rec loop = function
    | [] -> "]"
    | hd :: tl -> "[" ^ (string_of_value hd) ^ "; " ^ (loop tl)
  in loop (Array.to_list arr)

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
    )
in
let fundef1 =
  { name = Id.L "test_trace.1000"
  ; args = ["a.100"]
  ; fargs = []
  ; body = instr1
  ; ret = Type.Int
  }
in
let main =
  Ans (
    CallDir (Id.L ("f.10"), ["a.100"], [])
  )
in Prog ([], [fundef1], main)

let _ = run_test_tt_main begin
    "jit_interp_test" >::: [
      "find_red" >:: begin
        fun () ->
          let Prog (_, fundefs, _) = prog1 in
          let instr = match fundefs with
            | [fundef] -> fundef.body
            | _ -> failwith "Error."
          in
          let reds = ["a"; "b"] in
          assert_equal
            ~printer:print_string_list
            (find_in_body instr reds)
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
            (convert prog1 reg reds [])
            expected
      end;
      "colorize_red" >:: begin
        fun () ->
          let reg = Array.create 10 0 in
          reg.(2) <- 100;
          let args = ["bytecode.0"; "pc.1"; "a.2"] in
          let reds_index = [2] in
          let greens_index = [0; 1] in
          let expected = Array.create (Array.length reg) (Green (0)) in
          expected.(2) <- Red (100);
          assert_equal
            ~printer:string_of_value_array
            (JitInterop.colorize reg args reds_index greens_index)
            (expected)
      end
    ]
  end
