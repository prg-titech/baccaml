open Jit
open Runtime
open Config
module F = Filename

let () =
  let file = ref "" in
  Arg.parse
    [ ( "-no-jit"
      , Arg.Unit (fun _ -> set jit_flag `Off)
      , "disable jit compilation" )
    ; ( "-debug"
      , Arg.Unit (fun _ -> set log_level `Debug)
      , "enable debug mode" )
    ; ( "-info"
      , Arg.Unit (fun _ -> set log_level `Info)
      , "output info to stdout" )
    ; ( "-size"
      , Arg.Int (fun i -> set Internal.size i)
      , "change the size of virtual mems" )
    ; ( "-tj"
      , Arg.Unit (fun _ -> set jit_setup_mode `Method)
      , "run under a trace-based interpreter, and comile other functions \
         by method compilation" )
    ; ( "-mj"
      , Arg.Unit (fun _ -> set jit_setup_mode `Tracing)
      , "run under a method-based interpreter, and comile other functions \
         by tracing compilation" )
    ; ( "-all"
      , Arg.Unit (fun _ -> set jit_setup_mode `All)
      , "comile other functions by tracing and method compilation" )
    ; ( "-comp-only"
      , Arg.Unit (fun _ -> set comp_only_flag `Off)
      , "only compiling, not executing a resulting trace" )
    ]
    (fun f -> file := f)
    ("Usage: " ^ Sys.argv.(0) ^ " [-options] [your interp]");
  set file_name (Some !file);
  set reds [ "stack"; "sp"; "sp2"; "mode" ];
  set greens [ "pc"; "bytecode" ];
  Runtime_entry.callbacks ();
  Runtime_caml.callbacks ();
;;
