open RCaml
open MinCaml
open BacCaml
open Jit_config

let _ =
  run begin fun arg ->
    let { prog; reg; mem; red_args; ex_name } = prepare_env arg in
    let trace =
      Jit_tracing.run_while
        prog reg mem "min_caml_test_trace"
        (red_args @ ["bytecode"]) 3 0
    in
    print_endline (Emit_virtual.to_string_fundef trace);
    Jit_emit.emit_result_mj ~prog:prog ~traces:[trace] ~file:ex_name
  end
