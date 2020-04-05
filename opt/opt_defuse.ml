open Std
open MinCaml
open Asm
open Opt_lib

let f t =
  let rec const_fold t =
    let open Opt_const_fold in
    let open Opt_guard in
    const_fold_exp t
    |> const_fold_mov
    |> move_into_guard
    |> const_fold_id
  in
  let rec opt_mem t =
    Opt_mem.const_fold_rw t
    |> Opt_const_fold.elim_dead_exp
    |> Opt_const_fold.const_fold_mov
  in
  const_fold t |> opt_mem

let h { name; args; fargs; body; ret } =
  { name; args; fargs; body = f body; ret }
