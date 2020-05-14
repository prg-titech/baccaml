open Std
open MinCaml
open Asm

let is_pc x =
  let re = Str.regexp "^pc\\.[a-zA-Z0-9\\.]*" in
  Str.string_match re x 0
;;

let rec rename_guard pc tname env = function
  | Let ((var, typ), Set n, t) when is_pc var ->
    let env = M.add var n env in
    Let ((var, typ), Set n, rename_guard pc tname env t)
  | Let ((var, typ), Add (x, C y), t) when is_pc x ->
    let pc_v = M.find x env in
    let env = M.add var (pc_v + y) env in
    Let ((var, typ), Add (x, C y), rename_guard pc tname env t)
  | Let ((var, typ), Sub (x, C y), t) when is_pc x ->
    let pc_v = M.find x env in
    let env = M.add var (pc_v - y) env in
    Let ((var, typ), Add (x, C y), rename_guard pc tname env t)
  | Let ((var, typ), e, t) -> Let ((var, typ), e, rename_guard pc tname env t)
  | Ans (CallDir (Id.L name, args, fargs) as e) ->
    let pc_arg = List.find (fun arg -> M.mem arg env) args in
    Ans
      (match M.find_opt pc_arg env with
      | Some pc_v when pc_v = pc -> CallDir (Id.L tname, args, fargs)
      | Some _ | None -> e)
  | t -> t
;;
