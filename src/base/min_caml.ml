open Core
open MinCaml

let is_dump = ref `NG

let run_dump f =
  let inchan = In_channel.create (f ^ ".ml") in
  try
    Lexing.from_channel inchan
    |> Util.virtualize
    |> Trim.f
    |> Simm.f
    |> Emit_virtual.to_string_prog
    |> print_endline;
    In_channel.close inchan;
  with e ->
    In_channel.close inchan;
    raise e

let run_compile f =
  let inchan = In_channel.create (f ^ ".ml") in
  let outchan = Out_channel.create (f ^ ".s") in
  try
    Lexing.from_channel inchan
    |> Util.virtualize
    |> Trim.f
    |> Simm.f
    |> RegAlloc.f
    |> Emit.f outchan;
    In_channel.close inchan;
    Out_channel.close outchan;
  with e -> (In_channel.close inchan; Out_channel.close outchan; raise e)

let spec_list = [
  ("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
  ("-iter", Arg.Int(fun i -> Util.limit := i), "maximum number of optimizations iterated");
  ("-dump", Arg.Unit(fun _ -> is_dump := `OK), "emit virtual machine code");
]

let usage =
  "Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
  (Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..." Sys.argv.(0))

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let files = ref [] in
  Arg.parse spec_list begin fun f ->
    files := !files @ [String.split f ~on:'.'
                       |> List.hd
                       |> Option.value ~default:f]
  end usage;
  List.iter !files ~f:(match !is_dump with `OK -> run_dump | `NG -> run_compile)
