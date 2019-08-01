open Std
open Base
open Asm
open Jit_env
open Jit_util

let ignored x ys = ys |> List.exists (fun y -> String.get_name x = y)

let rec restore reg cont ?wlist:(ws = []) = function
  | [] -> cont
  | hd :: tl when not (ignored hd ws) ->
    begin match reg.(int_of_id_t hd) with
      | Green n when (
        String.get_name hd = "bytecode" ||
        String.get_name hd = "code") ->
        Let ( (hd, Type.Int)
            , CallDir (Id.L ("restore_min_caml_bp"), [], [])
            , restore reg cont tl)
      | Green n ->
        Let ( (hd, Type.Int)
            , Set n, restore reg cont tl)
      | _ -> restore reg cont tl
    end
  | hd :: tl -> restore reg cont tl

let rec jmp_to_guard tname t = match t with
  | Ans (CallDir (Id.L x, args, fargs)) when String.contains x "interp" ->
    Ans (CallDir (Id.L ("guard_" ^ tname), args, fargs))
  | Ans (e) -> Ans (e)
  | Let (x, e, t) -> Let (x, e, jmp_to_guard tname t)

let create reg tj_env ?wlist:(ws = []) cont =
  let free_vars = List.unique (fv cont) in
  restore reg cont free_vars

let create_tj reg tj_env ?wlist:(ws = []) cont =
  let { trace_name } = tj_env in
  let free_vars = List.unique (fv cont) in
  let t = restore reg cont free_vars in
  jmp_to_guard trace_name t
