open Asm
open Core

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

let is_red name reds =
  List.exists reds ~f:(fun n -> n = name)

let find_red_in_body t reds =
  let rec loop t reds acc = match t with
    | Let ((dest, _), _, body) ->
      if is_red dest reds then
        loop body reds (dest :: acc)
      else
        loop body reds acc
    | Ans (e) ->
      acc
  in List.rev (loop t reds []) |> remove_duplicates

let find_red_in_fundefs fundefs reds =
  List.fold_right
    ~f:(fun fundef env ->
        let { args; body } = fundef in
        let reds_in_args = List.filter ~f:(fun arg -> is_red arg reds) args in
        let reds_in_body = find_red_in_body body reds in
        remove_duplicates (List.append (List.append reds_in_body reds_in_args) env)
      )
    ~init:[]
    fundefs

let find_red_registers p reds =
  let Prog (_, fundefs, t) = p in
  List.append
    (find_red_in_body t reds)
    (find_red_in_fundefs fundefs reds)
