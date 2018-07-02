open MinCaml
open Asm
open Core
open Jit_config
open Jit_util

let remove_elt e l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x::xs when e = x -> go xs acc
    | x::xs -> go xs (x::acc)
  in go l []

let remove_duplicates l =
  let rec go l acc = match l with
    | [] -> List.rev acc
    | x :: xs -> go (remove_elt x xs) (x::acc)
  in go l []

let name_of id_t =
  match List.hd (String.split id_t ~on:'.') with
  | Some name -> name
  | None -> id_t

let is_red id_t reds =
  List.exists reds ~f:(fun n -> n = name_of id_t)

let find_red_in_exp exp reds = match exp with
  | Mov id_t | Neg id_t ->
    if is_red id_t reds then [id_t] else []
  | Add (id_t, id_or_imm) | Sub (id_t, id_or_imm) ->
    (match id_or_imm with
     | V (id_t') ->
       List.filter ~f:(fun id -> is_red id reds) [id_t; id_t']
     | C _ ->
       if is_red id_t reds then [id_t] else [])
  | Ld (id_t, id_or_imm, x) ->
    (match id_or_imm with
     | V (id_t') ->
       List.filter ~f:(fun id -> is_red id reds) [id_t; id_t']
     | C _ ->
       if is_red id_t reds then [id_t] else [])
  | St (id_t1, id_t2, id_or_imm, x) ->
    (match id_or_imm with
     | V (id_t') ->
       List.filter ~f:(fun id -> is_red id reds) [id_t1; id_t2; id_t']
     | C _ ->
       List.filter ~f:(fun id -> is_red id reds) [id_t1; id_t2])
  | IfEq (id_t, id_or_imm, _, _) | IfLE (id_t, id_or_imm, _, _) | IfGE (id_t, id_or_imm, _, _) ->
    (match id_or_imm with
     | V (id_t') ->
       List.filter ~f:(fun id -> is_red id reds) [id_t; id_t']
     | C _ ->
       if is_red id_t reds then [id_t] else [])
  | _ -> []

let find_red_in_body t reds =
  let rec loop t reds acc = match t with
    | Let ((dest, _), exp, body) ->
      let reds_in_exp = find_red_in_exp exp reds in
      if is_red dest reds then
        let r = List.rev_append (List.append reds_in_exp [dest]) acc in
        loop body reds r
      else
        loop body reds acc
    | Ans (e) ->
      acc
  in List.rev (loop t reds [])

let find_red_in_fundefs fundefs reds =
  List.fold_right
    ~f:(fun fundef env ->
        let { args; body } = fundef in
        let reds_in_args = List.filter ~f:(fun arg -> is_red arg reds) args in
        let reds_in_body = find_red_in_body body reds in
        List.append (List.append reds_in_body reds_in_args) env
      )
    ~init:[]
    fundefs

let find_red_registers p reds =
  let Prog (_, fundefs, t) = p in
  List.append
    (find_red_in_body t reds)
    (find_red_in_fundefs fundefs reds)

let convert p reg reds =
  let red_regs = find_red_registers p reds in
  let red_reg_num = List.map ~f:(fun id -> int_of_id_t id) red_regs in
  let jit_regs = Array.create (Array.length reg) (Green (0)) in
  Array.iter
    ~f:(fun n -> jit_regs.(n) <- Red (reg.(n)))
    (Array.of_list red_reg_num);
  jit_regs

