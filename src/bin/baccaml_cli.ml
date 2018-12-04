open Core
open MinCaml
open BacCaml

let level = ref `Dump

let interp = ref ""
let trace = ref ""
let jit_type = ref `Not_specified
let jit_type_str = ref ""

let cwd = Sys.getcwd ()

let get_prefix f = String.split f ~on:'.' |> List.hd |> Option.value ~default:f

let jit_typ typ = match typ with
  | "tjit" -> `Meta_tracing
  | "mjit" -> `Meta_method
  | _ -> `Not_specified

let tran_annot typ p =
  match typ with
  | `Meta_tracing -> Jit_annot.gen_mj false p
  | `Meta_method -> Jit_annot.gen_mj true p
  | `Not_specified -> failwith "jit type is not specified."

let dump typ file =
  In_channel.create file
  |> Lexing.from_channel
  |> Util.virtualize
  |> Trim.f
  |> Simm.f
  |> tran_annot typ
  |> Emit_virtual.to_string_prog
  |> Out_channel.print_endline

let compile typ f =
  let f = get_prefix f in
  let ic = In_channel.create (f ^ ".ml") in
  let oc = Out_channel.create (f ^ ".s") in
  try
    Lexing.from_channel ic
    |> Util.virtualize
    |> Trim.f
    |> Simm.f
    |> tran_annot typ
    |> RegAlloc.f
    |> Emit.f oc;
    In_channel.close ic;
    Out_channel.close oc;
  with e ->
    In_channel.close ic;
    Out_channel.close oc;
    raise e

exception No_such_file of string

let validate_file file =
  match Sys.is_file file with
  | `Yes -> ()
  | `No | `Unknown -> raise (No_such_file file)

let gen_interp_asm typ file =
  compile (jit_typ typ) file

let build_object_file file =
  let from = cwd ^ "/" ^ file ^ ".s" in
  let to' = cwd ^ "/" ^ file ^ ".o" in
  validate_file from;
  Printf.sprintf "gcc -c -g -m32 %s -o %s" from to'
  |> Sys.command_exn

let build_executable interp trace =
  let cmd =
    Printf.sprintf "gcc -g -m32 %s %s %s %s -o %s"
      ("stub/stub.c")
      ("stub/libmincaml.S")
      (interp ^ ".o")
      (trace ^ ".o")
      (trace)
  in
  Sys.command_exn cmd

let clean trace =
  let cmd = Printf.sprintf "rm -rf %s.dSYM" trace in
  Sys.command_exn cmd

let build typ interp trace =
  try
    gen_interp_asm typ interp;
    build_object_file interp;
    build_object_file trace;
    build_executable interp trace;
    clean trace
  with e ->
    Printf.eprintf "building %s %s is failed.\n" interp trace;
    raise e

let usage =
  "usage: " ^ Sys.argv.(0) ^ "[-interp] [-trace] [-dump] [-type (tjit|mjit)]"

let spec_list = [
  ("-dump", Arg.Unit (fun _ -> level := `Dump), "Dump intermadiate representation");
  ("-emit", Arg.Unit (fun _ -> level := `Emit), "Emit assembly file");
  ("-build", Arg.Unit (fun _ -> level := `Build), "Build executable");
  ("-interp", Arg.Set_string interp, "Specify interpreter file");
  ("-trace", Arg.Set_string trace, "Specify trace name");
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
  List.iter !files ~f:begin fun f ->
    match !level with
    | `Dump -> dump !jit_type f
    | `Emit -> compile !jit_type f
    | `Build -> build !jit_type_str (get_prefix f) !trace
  end
