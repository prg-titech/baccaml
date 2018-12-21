exception Error of string

let parse_from_channel ic =
  try
    let exps =
      Lexing.from_channel ic
      |> Bc_parser.program Bc_lexer.token
    in close_in ic; exps
  with e -> close_in ic; raise e

let array_of_exps exps =
  exps
  |> List.filter (function Sub.Inst _ -> true | _ -> false)
  |> List.map (function Sub.Inst i -> i | _ -> assert false)
  |> Array.of_list

type env = {
  green : (string * int) list;
  red : (string * int) list;
  jit_type : string;          (* tjit or mjit *)
  merge_pc : int;
  insts : int array
}

let env_of_exps exps =
  { green =
      exps
      |> List.filter (function Sub.Green _ -> true | _ -> false)
      |> List.map (function Sub.Green (v, i) -> (v, i) | _ -> assert false);
    red =
      exps
      |> List.filter (function Sub.Red _ -> true | _ -> false)
      |> List.map (function Sub.Red (v, i) -> (v, i) | _ -> assert false);
    jit_type =
      exps
      |> List.find (function Sub.Jit_type _ -> true | _ -> false)
      |> (function Sub.Jit_type (v) -> v | _ -> assert false);
    merge_pc =
      exps
      |> List.find (function Sub.Merge_pc _ -> true | _ -> false)
      |> (function Sub.Merge_pc (v) -> v | _ -> assert false);
    insts = array_of_exps exps
  }

let env_from_channel c =
  parse_from_channel c
  |> env_of_exps

let env_from_file file =
  let ic = open_in file in
  try
    let env = ic |> parse_from_channel |> env_of_exps in
    close_in ic; env
  with e -> close_in ic; raise e
