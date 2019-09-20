open Shoelib

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
