open Core
open MyFront
open Lexer
open Syntax
open Lexing
open Virtual
open Compiler

exception Interrupt of string
exception CompilationError of string
exception ConversionError of string

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.parse Lexer.token lexbuf with
  | SyntaxError msg ->
    fprintf stdout "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stdout "%a: syntax error\n" print_position lexbuf;
    None

let rec main_loop () =
  Printf.printf "ready> %!";
  try
    let line =
      match Out_channel.(flush stdout); In_channel.(input_line_exn stdin) with
      | s when s = "exit" -> exit 0
      | s -> s
      | exception End_of_file -> raise @@ Interrupt "\nKeyboard Interrupt."
    in
    let lexbuf = Lexing.from_string line in
    match parse_with_error lexbuf with
    | Some e ->
      Armin_impl.exec e;
      main_loop ()
    | None ->
      main_loop ()
  with
  | CompilationError s | ConversionError s ->
    print_endline s;
    main_loop ()
  | Invalid_argument s ->
    print_endline s;
    main_loop ()
  | Interrupt s ->
    print_endline s;
    exit 0
  | e ->
    print_endline "Anonymous Error.";
    main_loop ()

let _ =
  Logs.set_level @@ Some Logs.Debug;
  Logs.set_reporter @@ Logs.format_reporter ();
  main_loop ()
