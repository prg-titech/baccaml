open MinCaml
open BacCaml

let _ =
  let ic = open_in "./test/virtual_tracing_annot.ml" in
  let lexbuf = Lexing.from_channel ic in
  let p = Util.virtualize lexbuf |> Jit_annot.trans_mj |> Simm.f in
  Emit_virtual.to_string_prog p |> print_endline
