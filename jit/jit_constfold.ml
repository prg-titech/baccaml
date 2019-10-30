open Std
open MinCaml
open Asm

let (/=/) s1 s2 = String.(get_name s1 = get_name s2)

let rec replace trg = function
  | Ans (e) -> Ans (replace_exp trg e)
  | Let ((x, typ), e, t) -> Let ((x, typ), replace_exp trg e, replace trg t)

and replace_exp trg = function
  | Mov (x) when trg /=/ x -> Mov (trg)
  | Add (x, y) when x /=/ trg -> Add (trg, y)
  | Sub (x, y) when x /=/ trg -> Add (trg, y)
  | Mul (x, y) when x /=/ trg -> Add (trg, y)
  | IfEq (x, y, t1, t2) when x /=/ trg -> IfEq (trg, y, replace trg t1, replace trg t2)
  | IfLE (x, y, t1, t2) when x /=/ trg -> IfLE (trg, y, replace trg t1, replace trg t2)
  | IfGE (x, y, t1, t2) when x /=/ trg -> IfGE (trg, y, replace trg t1, replace trg t2)
  | IfEq (x, y, t1, t2) -> IfEq (x, y, replace trg t1, replace trg t2)
  | IfLE (x, y, t1, t2) -> IfLE (x, y, replace trg t1, replace trg t2)
  | IfGE (x, y, t1, t2) -> IfGE (x, y, replace trg t1, replace trg t2)
  | SIfEq (x, y, t1, t2) when x /=/ trg -> SIfEq (trg, y, replace trg t1, replace trg t2)
  | SIfLE (x, y, t1, t2) when x /=/ trg -> SIfLE (trg, y, replace trg t1, replace trg t2)
  | SIfGE (x, y, t1, t2) when x /=/ trg -> SIfGE (trg, y, replace trg t1, replace trg t2)
  | SIfEq (x, y, t1, t2) -> SIfEq (x, y, replace trg t1, replace trg t2)
  | SIfLE (x, y, t1, t2) -> SIfLE (x, y, replace trg t1, replace trg t2)
  | SIfGE (x, y, t1, t2) -> SIfGE (x, y, replace trg t1, replace trg t2)
  | CallCls (x, ys, zs) -> CallCls (x, List.map (fun y -> if y /=/ trg then trg else y) ys, List.map (fun z -> if z /=/ trg then trg else z) zs)
  | CallDir (x, ys, zs) -> CallDir (x, List.map (fun y -> if y /=/ trg then trg else y) ys, List.map (fun z -> if z /=/ trg then trg else z) zs)
  | exp -> exp

let can_folding trg = function
  | Mov (x) when trg /=/ x -> `Folding x
  | exp -> `Not_folding

let rec folding = function
  | Ans (e) -> Ans (e)
  | Let ((x, typ), e, t) ->
    match can_folding x e with
    | `Folding x -> replace x t
    | `Not_folding -> Let ((x, typ), e, folding t)

let rec iter ?(n=1) t =
  let r = folding t in
  if t = r then r else iter ~n:(n-1) r
