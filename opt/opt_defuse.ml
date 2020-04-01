open Std
open MinCaml
open Asm
open Opt_lib

let f t =
  Opt_const_fold.(
    const_fold_exp t
    |> const_fold_mov
    |> Opt_guard.move_into_guard
    |> const_fold_if
    |> const_fold_id
    |> elim_dead_exp)
  |> Opt_mem.const_fold_rw
  |> Opt_const_fold.elim_dead_exp
  |> Opt_const_fold.const_fold_mov
