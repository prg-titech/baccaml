open MinCaml
open Asm
open Operands
open Jit_util

exception Error

let rec annotate_t is_mj t = match t with
  | Ans (e) ->
    begin match e with
      | IfEq (x, y, t1, t2) | IfGE (x, y, t1, t2) | IfLE (x, y, t1, t2) ->
        Ans (e |%| (x, y, annotate_t is_mj t1, annotate_t is_mj t2))
      | IfFLE (x, y, t1, t2) | IfFEq (x, y, t1, t2) ->
        Ans (e |%| (x, V (y), annotate_t is_mj t1, annotate_t is_mj t2))
      | _ -> Ans (e)
    end
  | Let (x, CallDir (id_l, args, fargs), t) when id_l = (Id.L ("min_caml_is_mj"))->
    begin match t with
      | Ans (IfEq (_, _, t1, t2)) ->
        (* if is_mj () then t1 else t2 is compiled to *)
        (* IfEq((x, 0, t2, t1)                        *)
        begin
          match is_mj with
          | `Meta_method -> t2
          | `Meta_tracing -> t1
        end
      | _ ->
        Let (x, CallDir (id_l, args, fargs), annotate_t is_mj t)
    end
  | Let (r, x, t) ->
    Let (r, x, annotate_t is_mj t)

let rec annotate is_mj (Prog (table, fundefs, main) as p) =
  let { name; args; fargs; body; ret } = find_fundef' p "interp" in
  let other_fundefs = List.filter (fun fundef -> fundef.name <> name ) fundefs in
  let new_fundefs =
    { name = name;
      args = args;
      fargs = fargs;
      ret = ret;
      body = annotate_t is_mj body; } :: other_fundefs in
  Prog (table, new_fundefs, main)
