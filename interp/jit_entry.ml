open Utils
open Runtime
open Jit_runtime

let () =
  Arg.parse
    [("--no-jit", Arg.Unit (fun _ -> Config.jit_flag := `Off), "disable jit compilation");
     ("--debug", Arg.Unit (fun _ -> Log.log_level := `Debug), "enable debug mode")]
    (fun file -> Config.file_name := Some file)
    ("Usage: " ^ Sys.argv.(0) ^ " [--options] [your interp]");
  callbacks ()
