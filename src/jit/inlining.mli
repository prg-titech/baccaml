open MinCaml
val inline_args :
  Core.String.t list ->
  Core.String.t list -> Asm.t -> 'a Core.Array.t -> Asm.t
val inline_calldir_exp :
  Core.String.t list -> Asm.fundef -> 'a Core.Array.t -> Asm.t
