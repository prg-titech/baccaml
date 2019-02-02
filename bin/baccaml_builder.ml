
open MinCaml
open Bc_jit

let level = ref `Dump

let out = ref "a.out"
let interp = ref ""
let trace = ref ""
let traces = ref []
let jit_type = ref `Not_specified
let jit_type_str = ref ""

let cwd = Sys.getcwd ()

let get_prefix f =
  String.split_on_char '.'  f |> List.hd

let jit_typ typ = match typ with
  | "tjit" -> `Meta_tracing
  | "mjit" -> `Meta_method
  | _ -> `Not_specified

let tran_annot typ p =
  match typ with
  | `Meta_tracing | `Meta_method as typ -> Bc_lib.annot typ p
  | `Not_specified -> failwith "jit type is not specified."

let dump typ file =
  open_in file
  |> Lexing.from_channel
  |> Util.virtualize
  |> Trim.f
  |> Simm.f
  |> tran_annot typ
  |> Emit_virtual.string_of_prog
  |> print_endline

let compile typ f =
  let f = get_prefix f in
  let ic = open_in (f ^ ".ml") in
  let oc = open_out (f ^ ".s") in
  try
    Lexing.from_channel ic
    |> Util.virtualize
    |> Trim.f
    |> Simm.f
    |> tran_annot typ
    |> RegAlloc.f
    |> Emit.f oc;
    close_in ic;
    close_out oc;
  with e ->
    close_in ic;
    close_out oc;
    raise e

exception No_such_file of string


let gen_interp_asm typ file =
  compile (jit_typ typ) file

let build_object_file file =
  let from = cwd ^ "/" ^ file ^ ".s" in
  let to' = cwd ^ "/" ^ file ^ ".o" in
  Printf.sprintf "gcc -g -c -m32 %s -o %s" from to'
  |> Sys.command

let build_executable interp trace =
  let cmd =
    Printf.sprintf "gcc -g -m32 %s %s %s %s -o %s"
      ("stub/stub.c")
      ("stub/libmincaml.S")
      (interp ^ ".o")
      (trace ^ ".o")
      (trace)
  in
  Sys.command cmd

let build_executables interp traces out =
  let str_obj_of_list strs =
    List.fold_left (fun acc str -> acc ^ " " ^ str ^ ".o") "" strs
  in

  let cmd =
    Printf.sprintf "gcc -g -m32 %s %s %s %s -o %s"
      ("stub/stub.c")
      ("stub/libbaccaml.S")
      (interp ^ ".o")
      (str_obj_of_list traces)
      (out)
  in Sys.command cmd

let clean trace =
  let cmd = Printf.sprintf "rm -rf %s.dSYM" trace in
  Sys.command cmd

let build typ interp trace =
  try
    gen_interp_asm typ interp;
    build_object_file interp |> ignore;
    build_object_file trace |> ignore;
    build_executable interp trace |> ignore;
    clean trace
  with e ->
    Printf.eprintf "building %s %s is failed.\n" interp trace;
    raise e

let builds typ interp traces out =
  try
    gen_interp_asm typ interp;
    build_object_file interp |> ignore;
    traces |> List.iter (fun f -> build_object_file f |> ignore);
    build_executables interp traces out |> ignore;
    traces |> List.iter (fun t -> clean t |> ignore);
  with e ->
    Printf.eprintf "building %s is failed.\n" interp

let usage =
  "usage: " ^ Sys.argv.(0) ^ "[-interp] [-trace] [-dump] [-type (tjit|mjit)]"

let spec_list = [
  ("-dump", Arg.Unit (fun _ -> level := `Dump), "Dump intermadiate representation");
  ("-emit", Arg.Unit (fun _ -> level := `Emit), "Emit assembly file");
  ("-build", Arg.Unit (fun _ -> level := `Build), "Build executable");
  ("-interp", Arg.Set_string interp, "Specify interpreter file");
  ("-trace", Arg.String (fun f -> traces := !traces @ [f]), "Specify trace name");
  ("-o", Arg.Set_string out, "Output file name");
  ("-type", Arg.String begin fun arg ->
      jit_type_str := arg;
      match arg with
      | "tjit" -> jit_type := `Meta_tracing
      | "mjit" -> jit_type := `Meta_method
      | _ -> jit_type := `Not_specified
    end, "Specify jit type");
]

let _ =
  let files = ref [] in
  Arg.parse spec_list (fun s -> files := !files @ [s]) usage;
  !files |> List.iter begin fun f ->
    match !level with
    | `Dump -> dump !jit_type f
    | `Emit -> compile !jit_type f
    | `Build -> builds !jit_type_str (get_prefix f) !traces !out
  end
