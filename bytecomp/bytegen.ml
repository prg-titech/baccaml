open Bytegen_lib
open Virtual

let usage = "Usage: " ^ Sys.argv.(0) ^ "[-ast] [-virtual]"
let ast_flg = ref false
let virtual_flg = ref false

let tap f x = f x; x

let print_code arg =
  let print_code code =
    let count = ref 1 in
    Array.iteri (fun i elem ->
        if !count = 0 then
          begin
            count := 1;
            Printf.printf "code.(%d) <- %d;\n" i elem
          end
        else
          begin
            decr count;
            Printf.printf "code.(%d) <- %d;" i elem
          end
      ) code in
  let print_insts insts =
    Array.iter (fun inst ->
        Printf.printf "%s\n" (VM.show_inst inst))
      insts in
  let ic = open_in arg in
  try
    Lexing.from_channel ic
    |> Parser.exp Lexer.token
    |> Compiler.compile_from_exp
    |> tap (fun insts ->
        if !virtual_flg then print_insts insts)
    |> Array.map (fun elem -> elem |> VM.int_of_inst)
    |> print_code;
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
    (fun file -> print_code file)
    !files
