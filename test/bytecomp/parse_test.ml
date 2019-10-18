open OUnit2
open Bytegen_lib
open Syntax
open Parser
open Lexer

exception Test_failed of string

let exec_test code =
  try
    Lexing.from_string code
    |> Parser.exp Lexer.token
    |> Syntax.show_exp
    |> print_endline
  with e ->
    raise (Test_failed code)

let test_array _ =
  let code =
    "let arr = Array.make 10 0 in
     arr.(0) <- 3;
     let () = print_int (arr.(0))" in
  exec_test code

let suite =
  "Test Parsing" >::: [
      "test_array" >::
        (fun ctx -> test_array ctx);
    ]

let () =
  run_test_tt_main suite
