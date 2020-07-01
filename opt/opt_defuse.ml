open Std
open MinCaml
open Asm
open Opt_lib

let exec t =
  let rec const_fold t =
    Opt_const_fold.const_fold_exp t
    |> Opt_const_fold.const_fold_mov
    (* |> Opt_guard.move_into_guard *)
    |> Opt_const_fold.const_fold_id
  in
  let rec opt_mem t =
    Opt_mem.const_fold_rw t
    |> Opt_const_fold.elim_dead_exp
    |> Opt_const_fold.const_fold_mov
  in
  let rec loop i t =
    if i = 0 then t
    else const_fold t
  in
  let rec loop' i t =
    if i = 0 then t
    else opt_mem t
  in
  loop 100 t |> loop' 100
let h { name; args; fargs; body; ret } =
  { name; args; fargs; body = exec body; ret }
