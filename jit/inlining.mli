open MinCaml
open Jit_env

val inline_fundef' : reg -> string list -> Asm.fundef -> Asm.t * reg
val inline_fundef : reg -> string list -> Asm.fundef -> Asm.t
