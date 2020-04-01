open MinCaml
open Asm

val const_fold_mov : ?env:string M.t -> t -> t
val elim_dead_exp : t -> t
val const_fold : ?env:exp M.t -> t -> t
val const_fold_if : ?env:exp M.t -> t -> t
