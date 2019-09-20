open Shoelib
open Syntax

let test s =
  Lexing.from_string s
  |> Parser.exp Lexer.token
  |> Syntax.show_exp
  |> print_endline

let _ =
  let test1 = "let rec f x = x + 1 in f 1" in
  let test2 = "
let rec f x = x + 1 in
let rec g y = y + 100 in
let () = f (g (10))
" in
  test test1;
  test test2

let _ =
  if Array.length Sys.argv < 2 then
    prerr_endline "argument isn't specified"
  else
    let arg1 = Sys.argv.(1) in
    let ic = open_in arg1 in
    try
      Lexing.from_channel ic
      |> Parser.exp Lexer.token
      |> Syntax.show_exp
      |> print_endline;
      close_in ic;
    with e ->
      close_in ic; raise e
