open Runtime
open Config

let () =
  Arg.parse
    [("--no-jit", Arg.Unit (fun _ -> set jit_flag `Off), "disable jit compilation");
     ("--debug", Arg.Unit (fun _ -> set log_level `Debug), "enable debug mode");
     ("--info", Arg.Unit (fun _ -> set log_level `Info), "output info to stdout")]
    (fun file -> ())
    ("Usage: " ^ Sys.argv.(0) ^ " [--options] [your interp]");
  set file_name (Some (Filename.dirname Sys.argv.(0) ^ "/test_interp.mcml"));
  set reds (["stack"; "sp"; "sp2"; "mode"]);
  set greens (["pc"; "bytecode"]);
  Jit_runtime.callbacks ()
