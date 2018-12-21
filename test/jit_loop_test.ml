open Bc_lib
open MinCaml
open Bc_jit
open Jit_config

let _ =
  run begin fun jittype arg ->
    let { prog; reg; mem; red_args; ex_name } = prepare_env `Meta_tracing arg in

    let traces =
      Jit_method.run_while
        prog reg mem
        "min_caml_test_trace"
        red_args in

    List.iter (fun fundef ->
        Logs.debug (fun m ->
            m "%s" (Emit_virtual.to_string_fundef fundef))) traces;

    Jit_emit.emit_result ~prog:prog ~traces:traces ~file:ex_name ~jit_type:`Meta_tracing
  end
