open Core
open MinCaml
open BacCaml

let run_typ = ref `Emit

let jit_typ = ref `Not_specified

let id x = x

let annot p = match !jit_typ with
  | `Not_specified -> p
  | `Meta_method | `Meta_tracing as typ -> Jit_annot.gen_mj typ p

let run_dump f =
  let inchan = In_channel.create (f ^ ".ml") in
  try
    Lexing.from_channel inchan
    |> Util.virtualize
    |> Trim.f
    |> Simm.f
    |> annot
    |> Emit_virtual.to_string_prog
    |> print_endline;
    In_channel.close inchan;
  with e ->
    In_channel.close inchan;
    raise e

let run_interp f =
  let ic = In_channel.create (f ^ ".ml") in
  try
    Lexing.from_channel ic
    |> Util.virtualize
    |> Trim.f
    |> Simm.f
    |> annot
    |> Interp.f
    |> string_of_int
    |> print_endline
  with e ->
    In_channel.close ic;
    raise e

let run_compile f =
  let inchan = In_channel.create (f ^ ".ml") in
  let outchan = Out_channel.create (f ^ ".s") in
  try
    Lexing.from_channel inchan
    |> Util.virtualize
    |> Trim.f
    |> Simm.f
    |> annot
    |> RegAlloc.f
    |> Emit.f outchan;
    In_channel.close inchan;
    Out_channel.close outchan
  with e -> (In_channel.close inchan; Out_channel.close outchan; raise e)

let spec_list = [
  ("-inline", Arg.Int(fun i -> Inline.threshold := i), "maximum size of functions inlined");
  ("-iter", Arg.Int(fun i -> Util.limit := i), "maximum number of optimizations iterated");
  ("-type", Arg.String(fun str -> match str with
       | "mjit" -> jit_typ := `Meta_method
       | "tjit" -> jit_typ := `Meta_tracing
       | _ -> ()), "specify jit type");
  ("-err", Arg.Unit(fun _ -> Logs.set_level (Some (Logs.Error))), "Specify loglevel as error");
  ("-debug", Arg.Unit(fun _ -> Logs.set_level (Some (Logs.Debug))), "Specify loglevel as debug");
  ("-dump", Arg.Unit(fun _ -> run_typ := `Dump), "emit virtual machine code");
  ("-interp", Arg.Unit(fun _ -> run_typ := `Interp), "run as interpreter");
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
  List.iter !files ~f:begin
    match !run_typ with
      `Dump -> run_dump
    | `Interp -> run_interp
    | `Emit -> run_compile
  end
