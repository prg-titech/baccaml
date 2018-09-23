open MinCaml
open BacCaml

let _ =
  let ic = open_in "./test/virtual_tracing_annot.ml" in
  let lexbuf = Lexing.from_channel ic in
  let p = Util.virtualize lexbuf |> Simm.f in
  let p_mj = p |> Jit_annot.trans_mj in
  let p_tj = p |> Jit_annot.trans_tj in
  print_endline "[FOR MJ]";
  Emit_virtual.to_string_prog p_mj |> print_endline;
  print_newline ();
  print_endline "[FOR TJ]";
  Emit_virtual.to_string_prog p_tj |> print_endline
