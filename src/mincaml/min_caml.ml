open MinCaml
open Mutil
open Core

let ev_flg = ref false

let run_dump f =
  let inchan = In_channel.create (f ^ ".ml") in
  try
    Lexing.from_channel inchan
    |> virtualize
    |> Trim.f
    |> Simm.f
    |> Emit_virtual.to_string_prog
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

(* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
let run_compile f =
  let inchan = In_channel.create (f ^ ".ml") in
  let outchan = Out_channel.create (f ^ ".s") in
  try
    compile outchan (Lexing.from_channel inchan);
    In_channel.close inchan;
    Out_channel.close outchan;
  with e -> (In_channel.close inchan; Out_channel.close outchan; raise e)

let spec_list = [
  ("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
  ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
  ("-dump", Arg.Unit(fun _ -> ev_flg := true), "emit virtual machine code");
]

let usage =
  "Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
  (Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0))

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse spec_list
    begin fun s ->
      let flst = String.split s ~on:'.' in
      if not (String.equal (List.last_exn flst) "ml") then
        failwith "No suffix. Please add [filename].ml."
      else
        files := !files @ [List.hd_exn flst]
    end
    usage;
  List.iter !files ~f:(fun f ->
      if !ev_flg then ignore (run_dump f)
      else ignore (run_compile f))
