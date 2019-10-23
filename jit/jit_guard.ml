open Std
open Base
open Asm
open Jit_env
open Jit_util

let ignored x ys = ys |> List.exists (fun y -> String.get_name x = y)

let rec ignore_hits = function
    Ans (exp) -> Ans (ignore_hits_exp exp)
  | Let (x, CallDir (Id.L (id), args, fargs), body)
       when id = "min_caml_can_enter_jit" || id = "min_caml_jit_merge_point" ->
     ignore_hits body
  | Let (x, exp, body) ->
     Let (x, ignore_hits_exp exp, ignore_hits body)

and ignore_hits_exp = function
  | IfEq (x, y, t1, t2) | SIfEq (x, y, t1, t2) ->
     IfEq (x, y, ignore_hits t1, ignore_hits t2)
  | IfGE (x, y, t1, t2) | SIfLE (x, y, t1, t2) ->
     IfGE (x, y, ignore_hits t1, ignore_hits t2)
  | IfLE (x, y, t1, t2) | SIfGE (x, y, t1, t2) ->
     IfLE (x, y, ignore_hits t1, ignore_hits t2)
  | exp -> exp

let rec restore reg ~args ?wlist:(ws = []) cont =
  match args with
  | [] -> cont |> ignore_hits
  | hd :: tl when not (ignored hd ws) ->
    begin match reg.(int_of_id_t hd) with
      | Green n when (
        String.get_name hd = "bytecode" ||
        String.get_name hd = "code") ->
        Let ( (hd, Type.Int)
            , CallDir (Id.L ("restore_min_caml_bp"), [], [])
            , restore reg tl cont)
      | Green n ->
        Let ( (hd, Type.Int)
            , Set n, restore reg tl cont)
      | _ -> restore reg tl cont
    end
  | hd :: tl -> restore reg tl cont

let rec jmp_to_guard tname = function
  | Ans (e) ->
    begin match e with
      | CallDir (Id.L (x), args, fargs) when String.contains x "interp" ->
        Ans (CallDir (Id.L ("guard_" ^ tname), args, fargs))
      | IfEq (x, y, t1, t2) | SIfEq (x, y, t1, t2) ->
        Ans (IfEq (x, y, jmp_to_guard tname t1, jmp_to_guard tname t2))
      | IfGE (x, y, t1, t2) | SIfGE (x, y, t1, t2) ->
        Ans (IfGE (x, y, jmp_to_guard tname t1, jmp_to_guard tname t2))
      | IfLE (x, y, t1, t2) | SIfLE (x, y, t1, t2) ->
        Ans (IfLE (x, y, jmp_to_guard tname t1, jmp_to_guard tname t2))
      | e -> Ans (e)
    end
  | Let (x, e, t) -> Let (x, e, jmp_to_guard tname t)

module TJ : sig
  val create : reg -> string -> ?wlist:'a list -> t -> t
end = struct
  let create reg trace_name ?wlist:(ws = []) cont =
    let free_vars = List.unique (fv cont) in
    let t = restore reg free_vars cont in
    jmp_to_guard trace_name t
end

module MJ : sig
  val create : reg -> env -> ?wlist:'a list -> t -> t
end = struct
  let rec create reg tj_env ?wlist:(ws = []) cont =
    let free_vars = List.unique (fv cont) in
    restore reg free_vars cont
end
