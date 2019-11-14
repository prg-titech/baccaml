open MinCaml
open Asm

val folding : t -> t
val iter : ?n:int -> t -> t
val iter_fundef : ?n:int -> fundef -> fundef
