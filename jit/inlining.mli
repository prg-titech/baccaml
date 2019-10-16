open Base

val inline_fundef : 'a array -> string list -> Asm.fundef -> Asm.t

val inline_fundef' : 'a array -> string list -> Asm.fundef -> Asm.t * 'a array
val inline_fundef : 'a array -> string list -> Asm.fundef -> Asm.t
