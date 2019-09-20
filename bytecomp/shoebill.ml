open Shoelib
open Syntax

let tap f x = f x; x

let test ~name s =
  let open Virtual in
  print_string "[test] "; print_endline name;
  Lexing.from_string s
  |> Parser.exp Lexer.token
  |> tap (fun ast -> Syntax.show_exp ast |> print_endline)
  |> Compiler.compile_from_exp
  |> Array.map VM.show_inst
  |> Array.to_list
  |> String.concat "\n" |> print_endline

let _ =
  let test1 = "
let rec f x = x + 1 in
let () = f 1" in
  let test2 = "
let rec f x = x + 1 in
let rec g y = y + 100 in
let () = f (g (10))
" in
  test ~name:"test1" test1;
  test ~name:"test2" test2

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
