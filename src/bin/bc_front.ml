let parse_stdin =
  Lexing.from_channel stdin
  |> Bc_parser.program Bc_lexer.token


let parse_file file =
  let ic = open_in file in
  try
    let exp = Lexing.from_channel ic
              |> Bc_parser.program Bc_lexer.token in
    close_in ic;
    exp
  with e -> close_in ic; raise e


let array_of_exps exps =
  exps
  |> List.map (function Sub.Inst i -> i)
  |> Array.of_list
