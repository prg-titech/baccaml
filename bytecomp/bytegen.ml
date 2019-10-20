open Bytegen_lib
open Virtual

let usage = "Usage: " ^ Sys.argv.(0) ^ "[-ast] [-virtual]"
let ast_flg = ref false
let virtual_flg = ref false

let tap f x = f x; x

let print_code code =
  Array.iteri (fun i elem ->
      Printf.printf "code.(%d) <- %d;\n" i elem) code

let print_insts insts =
  Array.iter (fun inst ->
      Printf.printf "%s\n" (VM.show_inst inst))
    insts

let emit_ast arg =
  let ic = open_in arg in
  try
    Lexing.from_channel ic
    |> Parser.exp Lexer.token
    |> Syntax.show_exp
    |> print_endline
  with e ->
    close_in ic; raise e

let emit_virtual arg =
  let ic = open_in arg in
  try
    Lexing.from_channel ic
    |> Parser.exp Lexer.token
    |> Compiler.compile_from_exp
    |> Array.iter (fun inst -> inst |> VM.show_inst |> print_endline)
  with e ->
    close_in ic; raise e


let emit_code arg =
  let ic = open_in arg in
  try
    Lexing.from_channel ic
    |> Parser.exp Lexer.token
    |> Compiler.compile_from_exp
    |> Array.to_list
    |> Util.print_code;
    close_in ic;
  with e ->
    close_in ic; raise e


let _ =
  let files = ref [] in
  Arg.parse
    [("-ast", Arg.Unit (fun _ -> ast_flg := true), "emit abstract syntax tree");
     ("-virtual", Arg.Unit (fun _ -> virtual_flg := true), "emit virtual machine instructions")]
    (fun file -> files := !files @ [file])
    usage;
  List.iter
    (if !ast_flg then emit_ast
     else if !virtual_flg then emit_virtual
     else emit_code)
    !files
