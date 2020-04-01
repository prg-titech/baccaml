open MinCaml
open Asm
open Opt_lib

val const_fold_exp : ?env:exp M.t -> t -> t
val const_fold_mov : ?env:string M.t -> t -> t
val const_fold_id : ?env:string M.t -> t -> t
val const_fold_if : ?env:exp M.t -> t -> t
val elim_dead_exp : t -> t
