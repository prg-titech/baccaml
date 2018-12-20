open Bc_front_lib

let () =
  let f= Sys.argv.(1) in
  let exps = Bc_parser.program Bc_lexer.token (Lexing.from_channel (open_in f)) in
  List.iter (fun exp -> Sub.print_exp exp; print_newline ()) exps
