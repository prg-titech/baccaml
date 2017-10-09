open Asm
open Util

exception Un_implemented_instruction of string

(* TODO: Split して数字を取り出す実装ではなく
 * レジスタ番号を string で与えるように実装を変更する
*)
let int_of_id_t (id : Id.t) : int =
  let splitted = Str.split (Str.regexp_string ".") id in
  match splitted with
  | [ "min_caml_hp" ] -> 1000
  | _ ->
    (match List.nth splitted 1 with
     | num -> int_of_string num
     | exception _ -> int_of_string (Str.string_after (List.hd splitted) 2))

let float_of_id_t id =
  let splitted = Str.split (Str.regexp_string ".") id in
  match List.nth splitted 1 with
   | num -> float_of_string num
   | exception _ -> float_of_string (Str.string_after (List.hd splitted) 2)

let int_of_id_or_imm = function
  | V (id_t) -> int_of_id_t id_t
  | C (n) -> n

let string_of_id_or_imm = function
  | V (id_t) -> id_t
  | C (n) -> string_of_int n

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
  let to_string_id_l = function Id.L s -> s in
  match prog with
  | Prog (_, fundefs, _) ->
    try
      List.find (fun fundef -> (to_string_id_l fundef.name) = name) fundefs
    with Not_found ->
      Logger.error (Printf.sprintf "CallCls %s" name);
      raise Not_found

(* 仮引数のレジスタに実引数がしまわれている reg_set を作る *)
let make_reg_set (reg_set : int array) (argst : Id.t list) (argsr : Id.t list) : int array =
  let regst = List.map int_of_id_t argst in
  let regsr = List.map int_of_id_t argsr in
  let rec set reg = function
    | [] -> reg
    | (x, y) :: tl ->
      let r = reg.(y) in
      Logger.debug (Printf.sprintf "make_reg_set x: %d y: %d r: %d" x y r);
      reg.(x) <- r; set reg tl
  in
  set reg_set (ListUtil.zip regst regsr)

let rec interp (program : prog) (instruction : Asm.t) (reg_set : int array) (mem : int array) : 'a =
  match instruction with
  | Ans exp ->
    let res = interp' program exp reg_set mem in
    Logger.debug ("Ans " ^ (string_of_int res));
    res
  | Let ((id, _), exp, body) ->
    let reg_num = int_of_id_t id in
    let res = interp' program exp reg_set mem in
    Logger.debug ("Let " ^ id ^ " " ^ "reg_num: " ^ (string_of_int reg_num) ^ "  " ^ "res:" ^ (string_of_int res));
    reg_set.(reg_num) <- res;
    interp program body reg_set mem
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
    Logger.debug ("SetL " ^ (s));
    int_of_id_t s
  | Mov id_t ->
    let res = reg_set.(int_of_id_t id_t) in
    Logger.debug ("Mov " ^ (string_of_int res));
    res
  | Add (id_t, id_or_imm) ->
    let r1 = int_of_id_t id_t in
    let r2 = int_of_id_or_imm id_or_imm in
    Logger.debug ("Add " ^ (string_of_int r1) ^ " " ^ (string_of_int r2));
    reg_set.(r1) + reg_set.(r2)
  | Sub (id_t, id_or_imm) ->
    let r1 = reg_set.(int_of_id_t id_t) in
    let r2 = reg_set.(int_of_id_or_imm id_or_imm) in
    Logger.debug ("Sub " ^ (string_of_int r1) ^ " " ^ (string_of_int r2));
    r1 - r2
  | Ld (id_t, id_or_imm, x) ->
    (* id_t + id_or_imm * x の番地から load *)
    let m = (int_of_id_t id_t) + (int_of_id_or_imm id_or_imm) * x in
    let res = mem.(m) in
    Logger.debug (Printf.sprintf "Ld %d" res);
    res
  | St (id_t1, id_t2, id_or_imm, x) ->
    (* id_t2 + id_or_imm * x の番地に id_t1 を store *)
    let src = int_of_id_t id_t1 in
    let m = (int_of_id_t id_t2) + (int_of_id_or_imm id_or_imm) * x in
    mem.(m) <- src;
    0
  | Comment _ -> 0
  | FNegD id_t ->
    let x' = int_of_id_t id_t in
    (- reg_set.(x'))
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
    Logger.debug ("IfEq " ^ (string_of_int r1) ^ " " ^ (string_of_int r2));
    if r1 = r2 then
      interp program t1 reg_set mem
    else
      interp program t2 reg_set mem
  | IfLE (id, id_or_imm, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id) in
    let r2 = reg_set.(int_of_id_or_imm id_or_imm) in
    Logger.debug ("IfLE " ^ id ^ (string_of_id_or_imm id_or_imm) ^ "r1:" ^ (string_of_int r1) ^ " r2:" ^ (string_of_int r2));
    if r1 <= r2 then
      interp program t1 reg_set mem
    else
      interp program t2 reg_set mem
  | IfGE (id, id_or_imm, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id) in
    let r2 = reg_set.(int_of_id_or_imm id_or_imm) in
    Logger.debug ("IfGE" ^ (string_of_int r1) ^ " " ^ (string_of_int r2));
    if r1 >= r2 then
      interp program t1 reg_set mem
    else
      interp program t2 reg_set mem
  | IfFEq (id1, id2, t1, t2) ->
    let r1 = reg_set.(int_of_id_t id1) in
    let r2 = reg_set.(int_of_id_t id2) in
    Logger.debug ("IfFEq " ^ (string_of_int r1) ^ " " ^ (string_of_int r2));
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
  | LdDF (id_t, id_or_imm, x) ->
    let m = (int_of_id_t id_t) + (int_of_id_or_imm id_or_imm) * x in
    mem.(m)
  | StDF (id_t1, id_t2, id_or_imm, x) ->
    let src = int_of_id_t id_t1 in
    let m = (int_of_id_t id_t2) + (int_of_id_or_imm id_or_imm) * x in
    mem.(m) <- src;
    0
  | CallCls (name, args, _) ->
    let fundef = lookup_by_id_t program name in
    let args' = fundef.args in
    let body' = fundef.body in
    let reg_set' = make_reg_set reg_set args args' in
    interp program body' reg_set' mem
  | CallDir (Id.L ("min_caml_print_int"), [arg], _) ->
    let v = reg_set.(int_of_id_t arg) in
    Logger.debug (Printf.sprintf "CallDir min_caml_print_int %d" v);
    print_int v; print_newline ();
    0
  | CallDir (Id.L ("min_caml_print_newline"), _, _) -> print_newline (); 0
  | CallDir (Id.L ("min_caml_truncate"), _, [farg]) -> raise (Un_implemented_instruction "min_caml_truncate is not implemented.")
  | CallDir (Id.L ("min_caml_create_array"), _, _ ) -> raise (Un_implemented_instruction "min_caml_create array is not implemented.")
  | CallDir (name, args, _) ->
    (* 仮引数: args' 実引数: args *)
    let fundef = lookup_by_id_l program name in
    let args' = fundef.args in
    let body' = fundef.body in
    let reg_set' = make_reg_set reg_set args' args in
    let Id.L s = name in Logger.debug (Printf.sprintf "CallDir %s" s);
    interp program body' reg_set' mem
  | _ -> raise (Un_implemented_instruction "Not implemented.")

let f (prog : prog) : unit =
  let reg = Array.make 10000 0 in
  let mem = Array.make 10000 0 in
  let instructions = match prog with
    | Prog (_, _, t) -> t
  in
  ignore (interp prog instructions reg mem)
