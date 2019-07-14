
open Std
open Base
open Asm
open Jit_env
open Jit_util

let create reg tj_env ?wlist:(ws = []) cont =
  let free_vars = List.unique (fv cont) in
  let ignored x ys = ys |> List.exists (fun y -> String.get_name x = y) in
  let rec restore cont = function
    | [] -> cont
    | hd :: tl when not (ignored hd ws) ->
       begin match reg.(int_of_id_t hd) with
       | Green n when (String.get_name hd = "bytecode" || String.get_name hd = "code") ->
          Let ( (hd, Type.Int)
              , CallDir (Id.L ("restore_min_caml_bp"), [], [])
              , restore cont tl)
       | Green n ->
          Let ( (hd, Type.Int)
              , Set n, restore cont tl)
       | _ -> restore cont tl
       end
    | hd :: tl -> restore cont tl
  in
  restore cont free_vars
