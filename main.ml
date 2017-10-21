let limit = ref 1000
let is_emit_virtual = ref false
let is_interpreter = ref false

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
    let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
    if e = e' then e else
      iter (n - 1) e'

let virtualize l =
  Id.counter := 0;
  Typing.extenv := M.empty;
  Parser.exp Lexer.token l
  |> Typing.f
  |> KNormal.f
  |> iter !limit
  |> Alpha.f
  |> Closure.f
  |> Virtual.f

let compile outchan l =
  if !is_emit_virtual then
    virtualize l
    |> EmitVirtual.g outchan
  else
    virtualize l
    |> Simm.f
    |> RegAlloc.f
    |> Emit.f outchan

(* 文字列をコンパイルして標準出力に表示する (caml2html: main_string) *)
let string s = compile stdout (Lexing.from_string s)

(* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
let compile_exec f =
  let inchan = open_in (f ^ ".ml") in
  let outchan =
    if !is_emit_virtual then
      open_out (f ^ ".dump")
    else
      open_out (f ^ ".s") in
  try
    compile outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let interp l = virtualize l |> Interp.f

let interp_exec f =
  let file = if String.contains f '.' then f else f ^ ".ml" in
  let inchan = open_in file in
  try
    interp (Lexing.from_channel inchan);
    close_in inchan;
  with e -> (close_in inchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
     ("-dump", Arg.Unit(fun _ -> is_emit_virtual := true), "emit virtual machine code");
     ("-interp", Arg.Unit(fun _ -> is_interpreter := true), "interpreter mode");
     ("-debug", Arg.Unit(fun _ -> Logger.log_level := Logger.Debug), "print debug messages")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] [-virtual] [-interp] [-debug]...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> if !is_interpreter then interp_exec f else ignore (compile_exec f))
    !files
