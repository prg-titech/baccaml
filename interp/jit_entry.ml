open Jit
open Config
module F = Filename

let () =
  let file = ref "" in
  Arg.parse
    [ ( "-no-jit"
      , Arg.Unit (fun _ -> set jit_flag `Off)
      , "disable jit compilation" )
    ; "-debug", Arg.Unit (fun _ -> set log_level `Debug), "enable debug mode"
    ; "-info", Arg.Unit (fun _ -> set log_level `Info), "output info to stdout"
    ; ( "-comp-only"
      , Arg.Unit (fun _ -> set comp_only_flag `Off)
      , "only compiling, not executing a resulting trace" )
    ]
    (fun f -> file := f)
    ("Usage: " ^ Sys.argv.(0) ^ " [-options] [your interp]");
  set file_name (Some (!file));
  set reds [ "stack"; "sp"; "sp2"; "mode" ];
  set greens [ "pc"; "bytecode" ];
  Runtime_entry.callbacks ()
;;
