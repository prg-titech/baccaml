open MinCaml

val elim : ?i:int -> Asm.t -> Asm.t

val elim_fundef : ?i:int -> Asm.fundef -> Asm.fundef
