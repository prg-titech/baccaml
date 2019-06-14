open Base
open Asm
open Operands

let get_prefix s =
  s |> (String.split_on_char '.') |> List.hd

let (=|=) lhs rhs =
  get_prefix lhs = get_prefix rhs

let (|>|) exp (lhs, rhs) =
  match exp with
  | Add _ -> Add (lhs, rhs)
  | Sub _ -> Sub (lhs, rhs)
  | _ -> failwith "Add or Sub should be come here."

let replace_xy' ~x ~y ~lhs ~rhs exp =
  match y with
  | V (y) ->
     begin match x =|= lhs, y =|= lhs with
     | true, true -> exp |>| (rhs, V (rhs))
     | true, false -> exp |>| (rhs, V (y))
     | false, true -> exp |>| (x, V (rhs))
     | false, false -> exp |>| (x, V (y))
     end
  | C (n) ->
     if x =|= lhs then exp |>| (rhs, C (n))
     else exp |>| (x, C (n))

let replace_xy ~lhs ~rhs exp = match exp with
  | Add (x, y) -> replace_xy' x y lhs rhs exp
  | Sub (x, y) -> replace_xy' x y lhs rhs exp
  | _ -> failwith "Add, Sub, If should be come here."

let replace_ld_st ~lhs ~rhs = function
  | Ld (x, V (y), z) ->
     let f = fun (id, b) -> if b then rhs else id in
     (f (x, x =|= lhs), f (y, y =|= lhs))
     |> fun (a, b) -> Ld (a, V (b), z)
  | Ld (x, C (i), n) ->
     if x =|= lhs then Ld (rhs, C (i), n) else Ld (x, C (i), n)
  | St (x, y, V (z), n) ->
     let f = fun (id, b) -> if b then rhs else id in
     (f (x, x =|= lhs), f (y, y =|= lhs),  f(z, z =|= lhs))
     |> fun (a, b, c) -> St (a, b, V (c), n)
  | St (x, y, C (i), n) ->
     let f = fun (id, b) -> if b then rhs else id in
     (f (x, x =|= lhs), f (y, y =|= lhs))
     |> fun (a, b) -> St (a, b, C (i), n)
  | _ -> assert false

let rec replace ~lhs ~rhs = function
  | Ans (exp) -> Ans (replace_exp ~lhs:lhs ~rhs:rhs exp)
  | Let ((x, typ), exp, t) ->
     Let ((x, typ), replace_exp lhs rhs exp, replace lhs rhs t)

and replace_exp ~lhs ~rhs exp = match exp with
  | Mov (x) when x =|= lhs -> Mov (rhs)
  | Neg (x) when x =|= lhs -> Neg (rhs)
  | Add (x, y) | Sub (x, y) -> replace_xy lhs rhs exp
  | IfEq (x, y, t1, t2) | IfGE (x, y, t1, t2) | IfLE (x, y, t1,t2) ->
     replace_if ~lhs:lhs ~rhs:rhs exp
  | Ld _ | St _ -> replace_ld_st lhs rhs exp
  | CallDir (id_l, args, fargs) ->
     CallDir (
         id_l,
         args |> List.map (fun a -> if a =|= lhs then rhs else a),
         fargs |> List.map (fun a -> if a =|= lhs then rhs else a))
  | CallCls (x, args, fargs) ->
     CallCls (
         (if x =|= lhs then rhs else x),
         args |> List.map (fun a -> if a =|= lhs then rhs else a),
         fargs |> List.map (fun a -> if a =|= lhs then rhs else a))
  | exp -> exp

and replace_if ~lhs ~rhs exp = match exp with
  | IfEq (x, y, t1, t2) | IfLE (x, y, t1, t2) | IfGE (x, y, t1, t2) ->
     begin match y with
     | V (y) ->
        begin match x =|= lhs, y =|= lhs with
        | true, true ->
           exp |%| (rhs, V (rhs), replace lhs rhs t1, replace lhs rhs t2)
        | true, false ->
           exp |%| (rhs, V (y), replace lhs rhs t1, replace lhs rhs t2)
        | false, true ->
           exp |%| (x, V (rhs), replace lhs rhs t1, replace lhs rhs t2)
        | false, false ->
           exp |%| (x, V (y), replace lhs rhs t1, replace lhs rhs t2)
        end
     | C (n) ->
        if x =|= lhs then
          exp |%| (rhs, C (n), replace lhs rhs t1, replace lhs rhs t2)
        else
          exp |%| (x, C (n), replace lhs rhs t1, replace lhs rhs t2)
     end
  | _ -> assert false

let rec iter i t =
  let rec f = function
    | Ans (IfEq (x, y, t1, t2)) -> Ans (IfEq (x, y, f t1, f t2))
    | Ans (IfGE (x, y, t1, t2)) -> Ans (IfGE (x, y, f t1, f t2))
    | Ans (IfLE (x, y, t1, t2)) -> Ans (IfLE (x, y, f t1, f t2))
    | Ans (exp) -> Ans (exp)
    | Let ((x, typ), Mov (y), t) when x =|= y -> replace ~lhs:x ~rhs:y t
    | Let ((x, typ), exp, t) -> Let ((x, typ), exp, f t)
  in
  if i = 0 then t
  else
    let t2 = f t in
    iter (i - 1) t2

let rec elim ?(i = 100000) t =
  iter i t

let rec elim_fundef ?(i = 100000) { name; args; fargs; body; ret } =
  { name; args; fargs; body = iter i body; ret }
