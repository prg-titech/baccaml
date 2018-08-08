open RCaml
open MinCaml
open BacCaml
open Jit_config

let _ =
  run begin fun arg ->
    let { prog; reg; mem; red_args; ex_name } = prepare_env arg in

    reg.(1000) <- Green (1000);

    let traces =
      Method_jit_loop.run_while
        prog reg mem
        "min_caml_test_trace"
        ("bytecode" :: "stack" :: red_args) in

    List.iter (
      fun fundef ->
        Logs.debug (fun m ->
            m "%s" (Emit_virtual.to_string_fundef fundef))) traces;

    Jit_emit.emit_result_mj ~prog:prog ~traces:traces ~file:ex_name
  end