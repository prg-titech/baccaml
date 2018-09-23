open MinCaml
open Asm
open Operands

let rec trans_mj_call_ret = function
  | Ans (IfEq (id_t, id_or_imm, t1, t2) as exp)
  | Ans (IfLE (id_t, id_or_imm, t1, t2) as exp)
  | Ans (IfGE (id_t, id_or_imm, t1, t2) as exp) -> Ans (exp |%%| (trans_mj_call_ret t1, trans_mj_call_ret t2))
  | Ans (exp) -> Ans (exp)
  | Let ((dest, typ), CallDir (Id.L ("min_caml_mj_call_start"), _, _), t) -> trans_mj_call_ret t
  | Let ((dest, typ), CallDir (Id.L ("min_caml_mj_ret_start"), _, _), t) -> trans_mj_call_ret t
  | Let ((dest, typ), exp, Let ((dest', typ'), CallDir (Id.L ("min_caml_mj_call_end"), _, _), t')) -> Ans (exp)
  | Let ((dest, typ), exp, Let ((dest', typ'), CallDir (Id.L ("min_caml_mj_ret_end"), _, _), t')) -> Ans (exp)
  | Let ((dest, typ), exp, t) -> Let ((dest, typ), exp, trans_mj_call_ret t)

let rec trans_tj_call_ret =
  let rec aux = function
    | Ans (exp) -> Ans (exp)
    | Let ((dest, typ), CallDir (Id.L ("min_caml_mj_call_end"), _, _), t) -> t
    | Let ((dest, typ), CallDir (Id.L ("min_caml_mj_ret_end"), _, _), t) -> t
    | Let ((dest, typ), exp, t) -> aux t
  in
  function
  | Ans (IfEq (id_t, id_or_imm, t1, t2) as exp)
  | Ans (IfLE (id_t, id_or_imm, t1, t2) as exp)
  | Ans (IfGE (id_t, id_or_imm, t1, t2) as exp) -> Ans (exp |%%| (trans_tj_call_ret t1, trans_tj_call_ret t2))
  | Ans (exp) -> Ans (exp)
  | Let ((dest, typ), CallDir (Id.L ("min_caml_mj_call_start"), _, _), t) -> aux t
  | Let ((dest, typ), CallDir (Id.L ("min_caml_mj_ret_start"), _, _), t) -> aux t
  | Let ((dest, typ), exp, t) -> Let ((dest, typ), exp, trans_tj_call_ret t)

let trans_mj (Prog (table, fundefs, main)) =
  let { name; args; fargs; body; ret } = find_fundef "interp" fundefs in
  let other_fundefs = List.filter (fun fundef -> fundef.name <> name ) fundefs in
  let new_fundefs = { name = name; args = args; fargs = fargs; ret = ret; body = trans_mj_call_ret body; } :: other_fundefs
  in Prog (table, new_fundefs, main)

let trans_tj (Prog (table, fundefs, main)) =
  let { name; args; fargs; body; ret } = find_fundef "interp" fundefs in
  let other_fundefs = List.filter (fun fundef -> fundef.name <> name ) fundefs in
  let new_fundefs = { name = name; args = args; fargs = fargs; ret = ret; body = trans_tj_call_ret body; } :: other_fundefs
  in Prog (table, new_fundefs, main)
