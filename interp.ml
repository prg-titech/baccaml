open Asm

module ListUtil = struct
  let rec zip lst1 lst2 = match lst1, lst2 with
    | [], _ -> []
    | _, [] -> []
    | (x::xs), (y::ys) -> (x, y) :: (zip xs ys)
  let unzip lst =
    let f (l, s) (x, y) = (x::l, y::s) in
    List.fold_left f ([],[]) (List.rev lst)
end

let int_of_id_t (id : Id.t) : int =
  int_of_string (List.nth (Str.split (Str.regexp_string ".") id) 1)

let int_of_id_or_imm = function
  | V (id_t) -> int_of_id_t id_t
  | C (n) -> n

let rec lookup (prog : prog) (name : Id.l) : fundef  =
  match prog with
  | Prog (_, fundefs, _) ->
    List.find (fun (fundef) -> fundef.name = name) fundefs

let rec lookup_by_id_t (prog : prog) (name : Id.t) : fundef =
  match prog with
  | Prog (_, fundefs, _) ->
    let get_fundef_name = function
      | Id.L (s) -> s
    in
    List.find (fun (fundef) -> (get_fundef_name fundef.name) = name) fundefs

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

let rec interp (program : prog) (instruction : Asm.t) (reg_set : int array) (mem : int array) : 'a =
  match instruction with
  | Ans exp -> interp_exp program exp reg_set mem
  | Let ((id, _), exp, body) ->
    let reg_num = int_of_id_t id in
    let res = interp_exp program exp reg_set mem in
    reg_set.(reg_num) <- res;
    interp program body reg_set mem
and interp_exp (program : prog) (exp' : exp) (reg_set : int array) (mem : int array) : 'a =
  match exp' with
  | Nop -> 0 (* TODO: Nop の場合の処理を考える *)
  | Set n -> n
  | SetL (Id.L (s)) -> int_of_string s
  | Mov id_t -> int_of_id_t id_t
  | Add (id_t, id_or_imm) ->
    let r1 = int_of_id_t id_t in
    let r2 = int_of_id_or_imm id_or_imm in
    reg_set.(r2) + reg_set.(r1)
  | Sub (id_t, id_or_imm) ->
    let r1 = int_of_id_t id_t in
    let r2 = int_of_id_or_imm id_or_imm in
    reg_set.(r2) - reg_set.(r1)
  | FMovD id_t -> int_of_id_t id_t
  | FAddD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg_set.(y') + reg_set.(x')
  | FSubD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg_set.(y') - reg_set.(x')
  | FMulD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg_set.(y') * reg_set.(x')
  | FDivD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg_set.(y') / reg_set.(x')
  | IfEq (id1, id_or_imm, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id1) in
    let r2 = reg_set.(int_of_id_or_imm id_or_imm) in
    if r1 = r2 then
      interp program t1 reg_set mem
    else
      interp program t2 reg_set mem
  | IfLE (id, id_or_imm, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id) in
    let r2 = reg_set.(int_of_id_or_imm id_or_imm) in
    if r1 <= r2 then
      interp program t1 reg_set mem
    else
      interp program t2 reg_set mem
  | IfGE (id, id_or_imm, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id) in
    let r2 = reg_set.(int_of_id_or_imm id_or_imm) in
    if r1 >= r2 then
      interp program t1 reg_set mem
    else
      interp program t2 reg_set mem
  | IfFEq (id1, id2, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id1) in
    let r2 = reg_set.(int_of_id_t id2) in
    if r1 = r2 then
      interp program t1 reg_set mem
    else
      interp program t2 reg_set mem
  | IfFLE (id1, id2, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id1) in
    let r2 = reg_set.(int_of_id_t id2) in
    if r1 <= r2 then
      interp program t1 reg_set mem
    else
      interp program t2 reg_set mem
  | CallCls (name, args, _) ->
    let fundef = lookup_by_id_t program name in
    let args' = fundef.args in
    let body' = fundef.body in
    let reg_set' = make_reg_set reg_set args args' in
    interp program body' reg_set' mem
  | CallDir (name, args, _) ->
    let fundef = lookup program name in
    (* 仮引数: args' 実引数: args *)
    let args' = fundef.args in
    let body' = fundef.body in
    let reg_set' = make_reg_set reg_set args args' in
    interp program body' reg_set' mem

let f (prog : Asm.prog) =
  let reg = Array.make 256 0 in
  let mem = Array.make 256 0 in
  let t' = match prog with Prog (_, _, t) -> t in
  interp prog t' reg mem
