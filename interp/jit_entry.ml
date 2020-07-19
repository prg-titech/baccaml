open Jit
open Runtime
open Config
module F = Filename

module V1 = struct
  let parser file =
    let open Deprecated in
    Arg.parse
      [ ( "-no-jit"
        , Arg.Unit (fun _ -> jit_flag := `Off)
        , "disable jit compilation" )
      ; ( "-debug"
        , Arg.Unit (fun _ -> Log.log_level := `Debug)
        , "enable debug mode" )
      ; ( "-size"
        , Arg.Int (fun i -> Internal.size := i)
        , "change the size of virtual mems" )
      ; ( "-tj"
        , Arg.Unit (fun _ -> jit_setup_mode := `Method)
        , "run under a trace-based interpreter, and comile other functions by \
           method compilation" )
      ; ( "-mj"
        , Arg.Unit (fun _ -> jit_setup_mode := `Tracing)
        , "run under a method-based interpreter, and comile other functions by \
           tracing compilation" )
      ; ( "-all"
        , Arg.Unit (fun _ -> jit_setup_mode := `All)
        , "comile other functions by tracing and method compilation" )
      ]
      (fun f -> file := f)
      ("Usage: " ^ Sys.argv.(0) ^ " [-options] [your interp]")
  ;;
end

module V2 = struct
  let parse file =
    Arg.parse
      [ ( "--no-jit"
        , Arg.Unit (fun _ -> jit_flag := `Off)
        , "Disable JIT compilation" )
      ; ( "--debug"
        , Arg.Unit (fun _ -> Log.log_level := `Debug)
        , "Enable debug mode" )
      ; ( "--hybrid"
        , Arg.String
            (fun msg ->
              match msg with
              | "tj" | "tracing" -> hybrid_flag := `TJ
              | "mj" | "method" -> hybrid_flag := `MJ
              | _ -> hybrid_flag := `Nothing)
        , "Enable hybridization" )
      ; ( "--no-opt"
        , Arg.Unit (fun _ -> opt_flag := `Off)
        , "Disable optimizations")
      ]
      (fun f -> file := f)
      ("Usage: " ^ Sys.argv.(0) ^ " [-options] [interp]")
  ;;
end

let () =
  let file = ref "" in
  V2.parse file;
  file_name :=
    if !file = "" then failwith "interpreter definition is not specified."
    else Some !file;
  reds := [ "stack"; "sp"; "sp2"; "mode" ];
  greens := [ "pc"; "bytecode" ];
  Runtime_caml.callbacks ();
;;
