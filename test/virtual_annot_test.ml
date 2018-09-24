open MinCaml
open BacCaml
open RCaml

let _ =
  let ic = open_in "./test/virtual_annot.ml" in
  let lexbuf = Lexing.from_channel ic in
  let p = Util.virtualize lexbuf |> Simm.f in
  let p_mj = p |> Jit_annot.trans_mj in
  let p_tj = p |> Jit_annot.trans_tj in
  print_endline "[FOR MJ]";
  Emit_virtual.to_string_prog p_mj |> print_endline;
  print_newline ();
  print_endline "[FOR TJ]";
  Emit_virtual.to_string_prog p_tj |> print_endline

let _ =
  run begin fun jittype arg ->
    let { prog; reg; mem; red_args; ex_name } = prepare_env jittype arg in
    let trace =
      Jit_tracing.run_while
        prog reg mem "min_caml_test_trace"
        (red_args @ ["bytecode"]) 3 0
    in
    print_endline (Emit_virtual.to_string_fundef trace);
    Jit_emit.emit_result ~prog:prog ~traces:[trace] ~file:ex_name ~jit_type:jittype
  end
