open Core

open Mincaml
open Util

let limit = ref 1000
let ev_flg = ref false
let interp_flg = ref false

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
    let e' = Beta.f e |> Assoc.f |> Inline.f |> ConstFold.f |> Elim.f in
    if e = e' then e else iter (n - 1) e'

let virtualize l =
  Id.counter := 0;
  Typing.extenv := M.empty;
  Parser.exp Lexer.token l
  |> Typing.f
  |> KNormal.f
  |> fun t -> if !ev_flg then print_endline @@ KNormal.show t; t
  |> iter !limit
  |> Alpha.f
  |> Closure.f
  |> fun prog -> if !ev_flg then print_endline @@ Closure.show_prog prog; prog
  |> Virtual.f

let interp l =
  virtualize l |> Interp.f

let interp_exec f =
  let inchan = In_channel.create (f ^ ".ml") in
  try ignore (interp (Lexing.from_channel inchan)); In_channel.close inchan;
  with e ->
    In_channel.close inchan; raise e

let dump_exec f =
  let inchan = In_channel.create (f ^ ".ml") in
  try
    Lexing.from_channel inchan
    |> virtualize   
    |> Trim.f
    |> Simm.f
    (* |> Emit_virtual.to_string_progg *)
    |> Asm.show_prog
    |> print_endline;
    In_channel.close inchan;
  with e ->
    In_channel.close inchan;
    raise e

let compile outchan l =
  virtualize l
  |> Trim.f
  |> Simm.f
  |> RegAlloc.f
  |> Emit.f outchan

(* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)
let string s = compile stdout (Lexing.from_string s)

(* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
let compile_exec f =
  let inchan = In_channel.create (f ^ ".ml") in
  let outchan = Out_channel.create (f ^ ".s") in
  try
    compile outchan (Lexing.from_channel inchan);
    In_channel.close inchan;
    Out_channel.close outchan;
  with e -> (In_channel.close inchan; Out_channel.close outchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
     ("-dump", Arg.Unit(fun _ -> ev_flg := true), "emit virtual machine code");
     ("-i", Arg.Unit(fun _ -> interp_flg := true), "execute as interpreter");
     ("-trim", Arg.Unit(fun _ -> Trim.flg := true), "trim jit dispatcher");
     ("-debug", Arg.Unit(fun _ -> Logger.log_level := Logger.Debug), "print debug messages")]
    begin fun s ->
      let flst = String.split s ~on:'.' in
      ignore (
        if not (String.equal (List.last_exn flst) "ml") then
          failwith "No suffix. Please add [filename].ml.");
      files := !files @ [List.hd_exn flst]
    end
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  if (List.length !files) = 0 then
    (Format.printf "No target files. Execute as `./min-caml [file]'.";
     exit 1)
  else
    List.iter
      ~f:begin fun f ->
        if !interp_flg then ignore (interp_exec f)
        else if !ev_flg then ignore (dump_exec f)
        else ignore (compile_exec f)
      end
      !files
