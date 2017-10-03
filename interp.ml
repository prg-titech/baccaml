open Asm

let rec nth l = function
    0 -> List.hd l
  | i -> nth (List.tl l) (i - 1)

let rec interp (prog : t) (reg_set : int array) : int =
  match prog with
  | Ans exp -> interp_exp exp reg_set
  | Let ((id, _), exp, body) ->
    let reg_num = int_of_string (nth (Str.split (Str.regexp_string ".") id) 1) in
    let res = interp_exp exp in
    reg_set.(reg_num) <- res;
    interp body reg_set
and interp_exp (exp' : exp) (reg_set : int array) : int =
  match exp' with
  | Add (id, id_or_imm) -> 1
  | IfEq (id1, V (id2), t1, t2) ->
    let r1 = reg_set.(id1) in
    let r2 = reg_set.(id2) in
    if r1 = r2 then
      iterp t1 reg_set
    else
      iterp t2 reg_set
  | CallDir (name, args, _) ->
    let lookup prog n = ??? in
    let fundef = lookup prog name in
    let args' = fundef.args in  (* 仮引数: args' 実引数: args *)
    let body' = fundef.body in
    (* 仮引数のレジスタに実引数がしまわれている reg_set を作る *)
    let new_reg_set = make_reg_set reg_set args args' in
    interp body' new_reg_set
