open MinCaml
open BacCaml
open Jit_config
open Util
open RCaml

let _ =
  run (fun arg ->
      let { prog; reg; mem; red_args; ex_name } = prepare_env arg in

      mem.(1000) <- Red (1000);

      let traces =
        Method_jit_loop.run_while
          prog reg mem
          "min_caml_test_trace"
          ("bytecode" :: "stack" :: red_args) in

      List.iter (
        fun fundef ->
          Emit_virtual.to_string_fundef fundef
          |> Logger.debug) traces;

      Jit_emit.emit_result_mj ~prog:prog ~traces:traces ~file:ex_name
    )
