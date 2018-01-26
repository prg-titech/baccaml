open Asm
open Core
open Jit_config

let select_branch e n1 n2 t1 t2 =
  match e with
  | IfEq _ ->
    if n1 = n2 then t1 else t2
  | IfLE _ ->
    if n1 <= n2 then t1 else t2
  | IfGE _ ->
    if n1 >= n2 then t1 else t2
  | _ ->
    failwith "Only IfEq, IfLE and IfGE should be come here."

let rec get_free_vars = function
  | Ans (exp) -> get_free_vars' exp
  | Let ((dest, _), e, t) -> List.append (dest :: (get_free_vars' e)) (get_free_vars t)

and get_free_vars' = function
  | Mov (id) -> [id]
  | Add (id_t, V (id)) | Sub (id_t, V (id)) ->  id_t :: id :: []
  | Add (id_t, C _) | Sub (id_t, C _) -> id_t :: []
  | Ld (dest, V (offset), _) -> dest :: offset :: []
  | Ld (dest, C (_), _) -> dest :: []
  | St (src, dest, V (offset), _) -> src :: dest :: offset :: []
  | St (src, dest, C (_), _) -> src :: dest :: []
  | IfEq (id_t1, V (id_t2), _, _) | IfLE (id_t1, V (id_t2), _, _) | IfGE (id_t1, V (id_t2), _, _) -> id_t1 :: id_t2 :: []
  | IfEq (id_t1, C (_), _, _) | IfLE (id_t1, C (_), _, _) | IfGE (id_t1, C (_), _, _) -> id_t1 :: []
  | CallDir (id_l, args, fargs) -> List.append args fargs
  | _ -> []

let restore_green reg cont =
  let rec unique list =
    let rec go l s =  match l with
        [] -> s
      | first :: rest ->
        if List.exists ~f:(fun e -> e = first) s
        then go rest s
        else go rest (s @ [first])
    in go list []
  in
  let free_vars = unique (get_free_vars cont) in
  let rec restore cont = function
    | [] ->
      cont
    | hd :: tl ->
      match reg.(int_of_id_t hd) with
      | Green n ->
        Let ((hd, Type.Int), Set (n), restore cont tl)
      | Red _ ->
        restore cont tl
  in restore cont free_vars
