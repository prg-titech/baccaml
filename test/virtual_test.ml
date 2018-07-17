open RCaml
open MinCaml
open BacCaml
open Jit_config
open Util

let main name ex_name code annot red_lst green_lst =
  let Env (p, reg, mem, red_args) =
    prepare_env name ex_name code annot red_lst green_lst in

  mem.(1000) <- Red (1000);

  let traces =
    Method_jit_loop.run_while
      p reg mem
      "min_caml_test_trace"
      ("bytecode" :: "stack" :: red_args) in

  List.iter (fun fundef ->
      Emit_virtual.to_string_fundef fundef
      |> Logger.debug) traces;

  Jit_emit.emit_result_mj ~prog:p ~traces:traces ~file:ex_name;
  ()

let _ =
  run main
