open Mincaml
open Mutil
open Util

open Core

let emit_virtual = ref false
let is_interp = ref false
let trim = ref false

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
    (if !trim then      
       Lexing.from_channel inchan
       |> virtualize   
       |> Trim.f         
       |> Simm.f
       |> Emit_virtual.to_string_prog
       |> print_endline
     else
       Lexing.from_channel inchan
       |> virtualize   
       |> Simm.f
       |> Emit_virtual.to_string_prog
       |> print_endline);
    In_channel.close inchan;
  with e ->
    In_channel.close inchan;
    raise e

let compile outchan l =
  if !trim then
    virtualize l
    |> Trim.f
    |> Simm.f
    |> RegAlloc.f
    |> Emit.f outchan
  else
    virtualize l
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
     ("-dump", Arg.Unit(fun _ -> emit_virtual := true), "emit virtual machine code");
     ("-i", Arg.Unit(fun _ -> is_interp := true), "execute as interpreter");
     ("-trim", Arg.Unit(fun _ -> trim := true), "trim jit dispatcher");
     ("-debug", Arg.Unit(fun _ -> Logger.log_level := Logger.Debug), "print debug messages")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0));
  if (List.length !files) = 0 then
    (Format.printf "No target files. Execute as `./min-caml [file]'.";
     exit 1)
  else
    List.iter
      ~f:begin fun f ->
        if !is_interp then ignore (interp_exec f)
        else if !emit_virtual then ignore (dump_exec f)
        else ignore (compile_exec f)
      end
      !files
