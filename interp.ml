open Asm
open Util

type instruction = Asm.t

let int_of_id_t (id : Id.t) : int =
  int_of_string (List.nth (Str.split (Str.regexp_string ".") id) 1)

let int_of_id_or_imm = function
  | V (id_t) -> int_of_id_t id_t
  | C (n) -> n

let rec lookup (program : prog) (name : Id.l) : fundef  =
  match program with
  | Prog (_, fundefs, _) ->
    List.find (fun (fundef) -> fundef.name = name) fundefs

(* 仮引数のレジスタに実引数がしまわれている reg_set を作る *)
let make_reg_set (reg_set : int array) (args_tmp : Id.t list) (args_real : Id.t list) : int array =
  let regs_tmp = List.map int_of_id_t args_tmp in
  let regs_real = List.map int_of_id_t args_real in
  let rec set = function
    | [] -> ()
    | (x, y) :: tl -> reg_set.(x) <- y; set tl
  in
  set (ListUtil.zip regs_tmp regs_real);
  reg_set

let rec interp (program : prog) (instr : instruction) (reg_set : int array) : 'a =
  match instr with
  | Ans exp -> interp_exp program exp reg_set
  | Let ((id, _), exp, body) ->
    let reg_num = int_of_id_t id in
    let res = interp_exp program exp reg_set in
    reg_set.(reg_num) <- res;
    interp program body reg_set
and interp_exp (program : prog) (exp' : exp) (reg_set : int array) : 'a =
  match exp' with
  | Nop -> 0 (* TODO: Nop の場合の処理を考える *)
  | Set n -> n
  | SetL (Id.L (s)) -> int_of_string s
  | Mov id_t -> int_of_id_t id_t
  | Neg id_t ->
    let n = int_of_id_t id_t in
    Complement.two_complement n
  | Add (id_t, id_or_imm) ->
    let r1 = int_of_id_t id_t in
    let r2 = int_of_id_or_imm id_or_imm in
    reg_set.(r2) + reg_set.(r1)
  | Sub (id_t, id_or_imm) ->
    let r1 = int_of_id_t id_t in
    let r2 = int_of_id_or_imm id_or_imm in
    reg_set.(r2) - reg_set.(r1)
  | IfEq (id1, V (id2), t1, t2) ->
    let r1 = reg_set.(int_of_id_t id1) in
    let r2 = reg_set.(int_of_id_t id2) in
    if r1 = r2 then
      interp program t1 reg_set
    else
      interp program t2 reg_set
  | CallDir (name, args, _) ->
    let fundef = lookup program name in
    (* 仮引数: args' 実引数: args *)
    let args' = fundef.args in
    let body' = fundef.body in
    let reg_set' = make_reg_set reg_set args args' in
    interp program body' reg_set'
