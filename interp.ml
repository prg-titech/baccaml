open Asm
open Util

exception Un_implemented_instruction of string

let register_size = 10000
let heap_pointer = register_size - 1

(* TODO: Split して数字を取り出す実装ではなく *)
(* レジスタ番号を string で与えるように実装を変更する *)
let int_of_id_t = function
  | "min_caml_hp" -> heap_pointer
  | id ->
    let splitted = Str.split (Str.regexp_string ".") id in
    (match List.nth splitted 1 with
     | num -> int_of_string num
     | exception _ -> int_of_string (Str.string_after (List.hd splitted) 2))

let float_of_id_t id =
  let splitted = Str.split (Str.regexp_string ".") id in
  match List.nth splitted 1 with
  | num -> float_of_string num
  | exception _ -> float_of_string (Str.string_after (List.hd splitted) 2)

let int_of_id_or_imm = function V (id_t) -> int_of_id_t id_t | C (n) -> n

let string_of_id_or_imm = function V (id_t) -> id_t | C (n) -> string_of_int n

let rec lookup_by_id_l (prog : prog) (name : Id.l) : fundef =
  match prog with
  | Prog (_, fundefs, _) ->
    try
      List.find (fun (fundef) -> fundef.name = name) fundefs
    with Not_found ->
      let Id.L s = name in
      Logger.error s;
      raise Not_found

let rec lookup_by_id_t (prog : prog) (name : Id.t) : fundef =
  let Prog (_, fundefs, _) = prog in
  try
    List.find (fun fundef -> (let Id.L s = fundef.name in s) = name) fundefs
  with e ->
    Logger.error (Printf.sprintf "CallCls %s" name); raise e

(* 仮引数のレジスタに実引数がしまわれている reg_set を作る *)
let make_reg_set (reg_set : 'a array) (args_tmp : Id.t list) (args_real : Id.t list) : 'a array =
  let regs_tmp = List.map int_of_id_t args_tmp in
  let regs_real = List.map int_of_id_t args_real in
  let arr = Array.make 10000 0 in
  ignore (
    List.map
      (fun (x, y) -> arr.(x) <- reg_set.(y))
      (ListUtil.zip regs_tmp regs_real));
  arr

let rec interp (program : prog) (instruction : Asm.t) (reg_set : int array) (mem : int array) : 'a =
  match instruction with
  | Ans exp ->
    let res = interp' program exp reg_set mem in
    Logger.debug ("Ans " ^ (string_of_int res));
    res
  | Let ((id, _), exp, body) ->
    let reg_num = int_of_id_t id in
    let res = interp' program exp reg_set  mem in
    Logger.debug(Printf.sprintf "Let (id: %s, reg_num: %d, res: %d)" id reg_num res);
    reg_set.(reg_num) <- res;
    interp program body reg_set  mem
and interp' (program : prog) (exp' : exp) (reg_set : int array) (mem : int array) : 'a =
  match exp' with
  | Nop ->
    0
  | Set n ->
    Logger.debug (Printf.sprintf "Set %d" n);
    n
  | Neg n ->
    let res = reg_set.(int_of_id_t n) in
    Logger.debug (Printf.sprintf "Neg %d" res);
    (- res)
  | SetL (Id.L (s)) ->
    let r = reg_set.(int_of_id_t s) in
    Logger.debug (Printf.sprintf "SetL (%s: %d)" s r);
    r
  | Mov ("min_caml_hp") ->
    Array.length reg_set - 1
  | Mov id_t ->
    let res = reg_set.(int_of_id_t id_t) in
    Logger.debug (Printf.sprintf "Mov (%s: %d)" id_t res);
    res
  | Add (id_t, id_or_imm) ->
    let r1, r2 = int_of_id_t id_t, int_of_id_or_imm id_or_imm in
    let v1, v2 = reg_set.(r1), reg_set.(r2) in
    Logger.debug (Printf.sprintf "Add (%d: %d, %d: %d)" r1 v1 r2 v2);
    v1 + v2
  | Sub (id_t, id_or_imm) ->
    let r1 = reg_set.(int_of_id_t id_t) in
    let r2 = reg_set.(int_of_id_or_imm id_or_imm) in
    Logger.debug ("Sub " ^ (string_of_int r1) ^ " " ^ (string_of_int r2));
    r1 - r2
  | Ld (id_t, id_or_imm, x) ->
    (* id_t + id_or_imm * x の番地から load *)
    let m = (int_of_id_t id_t) + (int_of_id_or_imm id_or_imm) * x in
    let res = mem.(m) in
    Logger.debug (Printf.sprintf "Ld (%d: %d)" m res);
    res
  | St ("min_caml_hp", id_t2, id_or_imm, x) ->
    let src = reg_set.(heap_pointer) in
    let m = (int_of_id_t id_t2) + (int_of_id_or_imm id_or_imm) * x in
    Logger.debug (Printf.sprintf "St (m: %d), res: %d" m src);
    mem.(m) <- src;
    0
  | St (id_t1, id_t2, id_or_imm, x) ->
    (* id_t2 + id_or_imm * x の番地に id_t1 を store *)
    let src = reg_set.(int_of_id_t id_t1) in
    let m = (int_of_id_t id_t2) + (int_of_id_or_imm id_or_imm) * x in
    Logger.debug (Printf.sprintf "St (m: %d), res: %d" m src);
    mem.(m) <- src;
    0
  | Comment _ -> 0
  | FNegD id_t ->
    let x' = int_of_id_t id_t in
    (- truncate freg_set.(x'))
  | FMovD id_t -> int_of_id_t id_t
  | FAddD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg_set.(x') + reg_set.(y')
  | FSubD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg_set.(x') - reg_set.(y')
  | FMulD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg_set.(x') * reg_set.(y')
  | FDivD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg_set.(x') / reg_set.(y')
  | IfEq (id1, id_or_imm, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id1) in
    let r2 = reg_set.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfEq (id1: %s, id2: %s, t1: %d, t2: %d)" id1 (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 = r2 then
      interp program t1 reg_set  mem
    else
      interp program t2 reg_set  mem
  | IfLE (id, id_or_imm, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id) in
    let r2 = reg_set.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfLE (id: %s, id_or_imm: %s, t1: %d, t2: %d)" id (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 <= r2 then
      interp program t1 reg_set  mem
    else
      interp program t2 reg_set  mem
  | IfGE (id, id_or_imm, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id) in
    let r2 = reg_set.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfGE (id1: %s, id2: %s, t1: %d, t2: %d)" id (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 >= r2 then
      interp program t1 reg_set  mem
    else
      interp program t2 reg_set  mem
  | IfFEq (id1, id2, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id1) in
    let r2 = reg_set.(int_of_id_t id2) in
    Logger.debug (Printf.sprintf "IfFEq (id1: %s id2: %s t1: %d, t2: %d)" id1 id2 r1 r2);
    if r1 = r2 then
      interp program t1 reg_set  mem
    else
      interp program t2 reg_set  mem
  | IfFLE (id1, id2, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id1) in
    let r2 = reg_set.(int_of_id_t id2) in
    if r1 <= r2 then
      interp program t1 reg_set  mem
    else
      interp program t2 reg_set  mem
  | LdDF (id_t, id_or_imm, x) ->
    let m = (int_of_id_t id_t) + (int_of_id_or_imm id_or_imm) * x in
    mem.(m)
  | StDF (id_t1, id_t2, id_or_imm, x) ->
    let src = reg_set.(int_of_id_t id_t1) in
    let m = (int_of_id_t id_t2) + (int_of_id_or_imm id_or_imm) * x in
    mem.(m) <- src;
    0
  | CallCls (name, args, _) ->
    let fundef = lookup_by_id_t program name in
    let reg_set' = make_reg_set reg_set (fundef.args) args in
    let res = interp program (fundef.body) reg_set'  mem in
    Logger.debug (Printf.sprintf "CallCls (name: %s) res: %d" name res);
    res
  | CallDir (Id.L ("min_caml_print_int"), [arg], _) ->
    let v = reg_set.(int_of_id_t arg) in
    Logger.debug (Printf.sprintf "CallDir min_caml_print_int %d" v);
    print_int v; 0
  | CallDir (Id.L ("min_caml_print_newline"), _, _) ->
    print_newline (); 0
  | CallDir (Id.L ("min_caml_truncate"), _, [farg]) ->
    reg_set.(int_of_id_t farg)
  | CallDir (Id.L ("min_caml_create_array"), _, _ ) -> raise (Un_implemented_instruction "min_caml_create array is not implemented.")
  | CallDir (name, args, _) ->
    (* fundef.args: 仮引数 args: 実引数 *)
    let fundef = lookup_by_id_l program name in
    let reg_set' = make_reg_set reg_set (fundef.args) args in
    let Id.L s = name in Logger.debug (Printf.sprintf "CallDir %s" s);
    interp program (fundef.body) reg_set'  mem
  | _ -> raise (Un_implemented_instruction "Not implemented.")

let f (prog : prog) : unit =
  let reg = Array.make register_size 0 in
  let mem = Array.make register_size 0 in
  let Prog (_, _, instructions) = prog in
  ignore (interp prog instructions reg mem)
