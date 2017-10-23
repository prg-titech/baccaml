let limit = ref 1000
let is_emit_virtual = ref false

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

let interp l = virtualize l |> Interp.f

let interp_exec f =
  let inchan = open_in (f ^ ".ml") in
  let outchan =
    if !is_emit_virtual then
      Some (open_out (f ^ ".dump"))
    else
      None
  in
  try
    (match outchan with
     | Some (out) ->
       EmitVirtual.g out (virtualize (Lexing.from_channel inchan));
       close_out out
     | None ->
       interp (Lexing.from_channel inchan));
    close_in inchan;
  with e ->
    close_in inchan;
    (match outchan with
       | Some (out) -> close_out out
       | None -> ());
    raise e

let () =
  let files = ref [] in
  Arg.parse
    [("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
     ("-iter", Arg.Int(fun i -> limit := i), "maximum number of optimizations iterated");
     ("-dump", Arg.Unit(fun _ -> is_emit_virtual := true), "emit virtual machine code");
     ("-debug", Arg.Unit(fun _ -> Logger.log_level := Logger.Debug), "print debug messages")]
    (fun s -> files := !files @ [s])
    ("Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] [-virtual] [-debug]...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> interp_exec f)
    !files
