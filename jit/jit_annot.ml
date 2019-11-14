open MinCaml
open Asm
open Operands
open Jit_util

exception Error

let rec annotate_t typ t = match t with
  | Ans (e) ->
    begin
      match e with
      | IfEq (x, y, t1, t2)
      | IfGE (x, y, t1, t2)
      | IfLE (x, y, t1, t2)
      | SIfEq (x, y, t1, t2)
      | SIfGE (x, y, t1, t2)
      | SIfLE (x, y, t1, t2) ->
        Ans (e |%| (x, y, annotate_t typ t1, annotate_t typ t2))
      | IfFLE (x, y, t1, t2)
      | IfFEq (x, y, t1, t2)
      | SIfFLE (x, y, t1, t2)
      | SIfFEq (x, y, t1, t2) ->
        Ans (e |%| (x, V (y), annotate_t typ t1, annotate_t typ t2))
      | _ -> Ans (e)
    end
  | Let (x, CallDir (id_l, args, fargs), t) when id_l = (Id.L ("min_caml_is_mj"))->
    begin
      match t with
      | Ans (IfEq (_, _, t1, t2)) ->
        (* `if typ () then t1 else t2' is compiled to IfEq((x, 0, t2, t1) *)
        begin
          match typ with
          | `Meta_method -> t2
          | `Meta_tracing -> t1
        end
      | _ ->
        Let (x, CallDir (id_l, args, fargs), annotate_t typ t)
    end
  | Let (r, x, t) -> Let (r, x, annotate_t typ t)

let annotate_fundef typ { name; args; fargs; body; ret } =
  let rec loop typ n body =
    if n = 0 then body
    else
      let t = annotate_t typ body in
      if body = t then body
      else loop typ (n-1) t
  in
  let body = loop typ 100 body in
  { name; args; fargs; body; ret }

let rec annotate typ (Prog (table, const, fundefs, main) as p) =
  let rec loop typ n body =
    if n = 0 then body
    else
      let t = annotate_t typ body in
      if body = t then body
      else loop typ (n-1) t
  in
  let { name; args; fargs; body; ret } = Fundef.find p (Id.L "interp") in
  let other_fundefs = List.filter (fun fundef -> fundef.name <> name ) fundefs in
  let new_fundefs =
    { name = name; args = args; fargs = fargs; ret = ret;
      body = loop typ 100 body; } :: other_fundefs in
  Prog (table, const, new_fundefs, main)
