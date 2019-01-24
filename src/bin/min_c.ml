open MinCaml
open Emit_c

(*Generates the intermediate code for use in debugging the translation*)
let debug s =
  let result = Lexing.from_string s
	       |> Parser.exp Lexer.token
	       |> Typing.f
	       |> KNormal.f
	       |> Assoc.f
	       |> Inline.f
	       |> Elim.f
	       |> Closure.f in result

(*Reads a file and translates the Min-Caml code*)
let main file =
  Format.eprintf "Preparing file %s.ml for translation...@." file;
  let lines = ref "" in
  let in_channel = open_in (file ^ ".ml") in
  try
    while true do
      lines := Printf.sprintf "%s%s\n" !lines (input_line in_channel)
    done;
  with End_of_file ->
    close_in in_channel;
    let result = translate !lines in
    let out_channel = open_out (file ^ ".ml.c") in
    Format.eprintf "Outputting to %s.ml.c...@." file;
    output_string out_channel result;
    close_out out_channel;
    Format.eprintf "Translation complete.@."

(*Get the execution time of the translator*)
let time f x =
  let start = Sys.time () in
  let res = f x in
  let () = Printf.eprintf "Translation Time: %.2fs\n%!" (Sys.time () -. start) in
  res

let () =
  if Array.length Sys.argv = 1
  then begin Format.printf "Usage: min-caml filename@."; exit 0 end
  else time main Sys.argv.(1)
