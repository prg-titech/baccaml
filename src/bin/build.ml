open Core

let interp = ref ""
let trace = ref ""

let cwd = Sys.getcwd ()

let build_object_file file =
  let from = cwd ^ "/" ^ file ^ ".s" in
  let to' = cwd ^ "/" ^ file ^ ".o" in
  let cmd = Printf.sprintf "gcc -c -g -m32 %s -o %s" from to' in
  Sys.command_exn cmd

let build_executable interp trace =
  let cmd =
    Printf.sprintf "gcc -g -m32 %s %s %s %s -o %s"
      (cwd ^ "/" ^ "stub/stub.c")
      (cwd ^ "/" ^ "stub/libbaccaml.S")
      (cwd ^ "/" ^ interp ^ ".o")
      (cwd ^ "/" ^ trace ^ ".o")
      (cwd ^ "/" ^ trace)
  in
  Sys.command_exn cmd

let clean trace =
  let cmd = Printf.sprintf "rm -rf %s.dSYM" trace in
  Sys.command_exn cmd

let build interp trace =
  try
    build_object_file interp;
    build_object_file trace;
    build_executable interp trace;
    clean trace
  with e ->
    Printf.eprintf "building %s %s is failed." interp trace;
    raise e

let usage = "[usage] " ^ Sys.argv.(0) ^ "[-interp] [-trace]"

let spec_list = [
  ("-interp", Arg.Set_string interp, "Specify interpreter file");
  ("-trace"), Arg.Set_string trace, "Specify trace name";
]

let _ =
  Arg.parse spec_list (fun _ -> ()) usage;
  build !interp !trace
