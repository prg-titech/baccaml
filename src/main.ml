open Mincaml
open Mincaml_util

module Logger = Util__.Logger

let is_emit_virtual = ref false

let is_interp = ref false

let compile outchan l =
  virtualize l
  |> Simm.f
  |> RegAlloc.f
  |> Emit.f outchan

let interp l =
  virtualize l |> Interp.f

let interp_exec f =
  let inchan = open_in (f ^ ".ml") in
  try ignore (interp (Lexing.from_channel inchan)); close_in inchan;
  with e ->
    close_in inchan; raise e

let dump_exec f =
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".dump") in
  try
    Lexing.from_channel inchan
    |> virtualize
    |> Emit_virtual.to_string_prog
    |> print_string
    |> print_newline;
    close_in inchan;
    close_out outchan;
  with e ->
    close_in inchan;
    close_out outchan;
    raise e

(* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)
let string s = compile stdout (Lexing.from_string s)

(* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
let compile_exec f =
  let inchan = open_in (f ^ ".ml") in
  let outchan = open_out (f ^ ".s") in
  try
    compile outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
     ("-dump", Arg.Unit(fun _ -> is_emit_virtual := true), "emit virtual machine code");
     ("-i", Arg.Unit(fun _ -> is_interp := true), "execute as interpreter");
     ("-debug", Arg.Unit(fun _ -> Logger.log_level := Logger.Debug), "print debug messages")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  if (List.length !files) = 0 then
    (Format.printf "No target files. Execute as `./min-caml [file]'.";
     exit 1)
  else
    List.iter
      begin fun f ->
        if !is_interp then ignore (interp_exec f)
        else if !is_emit_virtual then ignore (dump_exec f)
        else ignore (compile_exec f)
      end
      !files
