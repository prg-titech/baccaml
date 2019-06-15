open Runtime
open Jit_runtime

let () =
  Arg.parse
    [("--no-jit", Arg.Unit (fun _ -> Config.jit_flag := `Off), "disable jit compilation");
     ("--debug", Arg.Unit (fun _ -> Config.set_log_level `Debug), "enable debug mode")]
    (fun file -> ())
    ("Usage: " ^ Sys.argv.(0) ^ " [--options] [your interp]");
  Config.file_name := Some (Filename.dirname Sys.argv.(0) ^ "/test_interp.mcml");
  callbacks ()
