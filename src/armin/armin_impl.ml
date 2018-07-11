open Core
open Compiler

exception Error of string

let exec e =
  Logs.debug (fun m -> m "AST ==> %s" (Syntax.show_exp e));

  let insts =
    try compile_exp_entry empty_fenv e empty_env with
    | e -> raise @@ Error "compilation failed."
  in

  let insts' = resolve_labels insts in
  List.iter insts' (fun inst ->
      Logs.debug @@
      fun m -> m ~header:"INSTS" "%s "((VM.show_inst inst) ^ ";"));

  let insts_nums =  try List.map insts' Virtual.int_of_inst with
    | Failure msg -> raise @@ Error msg
    | e -> raise @@ Error "Conversion to instruction number is failed."
  in
  Logs.info (fun m ->
      m ~header:"BYTECODE" "[| %s |]"
        (String.concat ~sep:"; " (List.map ~f:string_of_int insts_nums)));

  let stack = Array.create (Virtual.max_stack_depth) 0 in
  let sp = 0 in
  let v = Virtual.interp (Array.of_list insts_nums) 0 (sp, stack) in
  print_endline @@ "==> " ^ (string_of_int v)

let entry ic =
  match Parser.parse Lexer.token (Lexing.from_channel (ic)) with
  | Some e -> exec e; In_channel.close ic
  | None -> In_channel.close ic
