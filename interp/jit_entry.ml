open Utils
open Runtime
open Jit_runtime

let main =
  Arg.parse
    [("--no-jit", Arg.Unit (fun _ -> Config.jit_flag := `Off), "disable jit compilation");
     ("--debug", Arg.Unit (fun _ -> Log.log_level := `Debug), "enable debug mode")]
    (fun file -> Config.file_name := Some file)
    ("Usage: " ^ Sys.argv.(0) ^ " [--options] [your interp]");
  Callback.register "jit_tracing_entry" jit_tracing_entry;
  Callback.register "jit_exec" jit_exec;
  Callback.register "jit_method_call" jit_method_call;
