open Shoelib
open Virtual

let print_code code =
  code |> Array.iteri begin fun i elem ->
    Printf.printf "code.(%d) <- %s;\n" i elem
  end

let _ =
  if Array.length Sys.argv < 2 then ()
  else
    let arg1 = Sys.argv.(1) in
    let ic = open_in arg1 in
    try
      Lexing.from_channel ic
      |> Parser.exp Lexer.token
      |> Compiler.compile_from_exp
      |> Array.map (fun elem -> elem |> VM.int_of_inst |> string_of_int)
      |> print_code;
      close_in ic;
    with e ->
      close_in ic; raise e
