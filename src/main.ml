open Mincaml
open Mutil
open Util

open Core

let emit_virtual_flg = ref false
let interp_flg = ref false

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

(* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)
let string s = compile stdout (Lexing.from_string s)

(* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
let compile_exec f =
  let flst = String.split ~on:'.' f in
  ignore (match List.last flst with
      | Some (v) when v = "ml" -> ()
      | _ -> failwith "No suffix or suffix is not .ml.");
  let f' = match flst |> List.hd with
    | Some (v) -> v
    | None -> failwith "No suffix. please add .ml"
  in
  let inchan = In_channel.create (f' ^ ".ml") in
  let outchan = Out_channel.create (f' ^ ".s") in
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
     ("-dump", Arg.Unit(fun _ -> emit_virtual_flg := true), "emit virtual machine code");
     ("-i", Arg.Unit(fun _ -> interp_flg := true), "execute as interpreter");
     ("-trim", Arg.Unit(fun _ -> Trim.flg := true), "trim jit dispatcher");
     ("-debug", Arg.Unit(fun _ -> Logger.log_level := Logger.Debug), "print debug messages")]
    begin fun s ->
      let flst = String.split s ~on:'.' in
      ignore (if List.last_exn flst <> "ml" then failwith "No suffix. Please add [filename].ml.");
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
        else if !emit_virtual_flg then ignore (dump_exec f)
        else ignore (compile_exec f)
      end
      !files
