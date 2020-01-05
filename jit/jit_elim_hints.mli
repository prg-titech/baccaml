open MinCaml
open Asm

val elim_hints : t -> t
val elim_hints_fundef : fundef -> fundef

val elim_hints_and_rename
  :  [< `Meta_tracing | `Meta_method ]
  -> fundef list
  -> fundef list
