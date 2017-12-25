open MincamlUtil

let is_emit_virtual = ref false

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
       ignore (interp (Lexing.from_channel inchan)));
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
    ("Min-Caml Interpreter (C) Yusuke Izawa\n" ^
     Printf.sprintf "usage: %s [-inline m] [-iter n] [-virtual] [-debug]...filenames without \".ml\"..." Sys.argv.(0));
  List.iter
    (fun f -> interp_exec f)
    !files
