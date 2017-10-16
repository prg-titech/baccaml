open Asm
open Util

exception Un_implemented_instruction of string

let register_size = 10000

let heap_pointer = register_size / 2

type labels = (Id.l * int) list (* function label for closures *)

type prog_interp = ProgInterp of (Id.l * float) list * fundef list * t * labels (* prog for interpreter *)

type register = int array

type memory = int array

let int_of_id_t = function (* TODO: レジスタ番号をsringで与える実装に変更 *)
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

let rec find_label_number label = function
  | [] -> let Id.L s = label in int_of_id_t s
  | (l, num) :: tl -> if l = label then num else find_label_number label tl

let rec find_closure prog name reg =
  match prog with
  | ProgInterp (_, fundefs, _, labels) ->
    try
      let label_num = find_label_number name labels in
      reg.(label_num)
    with Not_found ->
      let Id.L s = name in
      Logger.error s;
      raise Not_found

let rec lookup_by_id_l prog name =
  let ProgInterp (_, fundefs, _, _) = prog in
  try
    List.find (fun fundef -> (fundef.name = name)) fundefs
  with e ->
    Logger.error (let Id.L s = name in Printf.sprintf "CallCls %s" s); raise e

let rec lookup_by_id_t prog name =
  let ProgInterp (_, fundefs, _, _) = prog in
  try
    List.find (fun fundef -> (let Id.L s = fundef.name in s) = name) fundefs
  with e ->
    Logger.error (Printf.sprintf "CallCls %s" name); raise e

let make_reg reg args_tmp args_real = (* 仮引数のレジスタに実引数がしまわれている reg を作る *)
  let regs_tmp = List.map int_of_id_t args_tmp in
  let regs_real = List.map int_of_id_t args_real in
  let arr = Array.make 10000 0 in
  ignore (
    List.map
      (fun (x, y) -> arr.(x) <- reg.(y))
      (List.zip regs_tmp regs_real));
  arr

let rec interp (prog : prog_interp) (instruction : Asm.t) (reg : register) (mem : memory) : 'a =
  match instruction with
  | Ans exp ->
    let res = interp' prog exp reg mem in
    Logger.debug (Printf.sprintf "Ans (%d)" res);
    res
  | Let ((id, _), exp, body) ->
    let reg_num = int_of_id_t id in
    let res = interp' prog exp reg mem in
    Logger.debug(Printf.sprintf "Let (id: %s, reg_num: %d, res: %d)" id reg_num res);
    reg.(reg_num) <- res;
    interp prog body reg  mem

and interp' (prog : prog_interp) (exp' : exp) (reg : register) (mem : memory) : 'a =
  match exp' with
  | Nop ->
    0
  | Set n ->
    Logger.debug (Printf.sprintf "Set %d" n);
    n
  | Neg n ->
    let res = reg.(int_of_id_t n) in
    Logger.debug (Printf.sprintf "Neg %d" res);
    (- res)
  | SetL (Id.L (s)) ->
    let ProgInterp (_, _, _, labels) = prog in
    let res = find_label_number (Id.L (s)) labels in
    Logger.debug (Printf.sprintf "SetL (%s: %d)" s res);
    res
  | Mov id_t ->
    let res = reg.(int_of_id_t id_t) in
    Logger.debug (Printf.sprintf "Mov (%s: %d)" id_t res);
    res
  | Add (id_t1, V (id_t2)) ->
    let r1, r2 = int_of_id_t id_t1, int_of_id_t id_t2 in
    let v1, v2 = reg.(r1), reg.(r2) in
    Logger.debug (Printf.sprintf "Add (%s %d: %d, %s %d: %d)" id_t1 r1 v1 id_t2 r2 v2);
    v1 + v2
  | Add (id_t, C n) ->
    let r1 = int_of_id_t id_t in
    let v1 = reg.(r1) in
    Logger.debug (Printf.sprintf "AddImm (reg: %d, val: %n, imm: %d)" r1 v1 n);
    v1 + n
  | Sub (id_t, id_or_imm) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = reg.(int_of_id_or_imm id_or_imm) in
    Logger.debug ("Sub " ^ (string_of_int r1) ^ " " ^ (string_of_int r2));
    r1 - r2
  | Ld (id_t, id_or_imm, x) ->
    (* id_t + id_or_imm * x の番地から load *)
    let m = (int_of_id_t id_t) + (int_of_id_or_imm id_or_imm) * x in
    let res = mem.(m) in
    Logger.debug (Printf.sprintf "Ld (%d: %d)" m res);
    res
  | St (id_t1, id_t2, id_or_imm, x) ->
    (* id_t2 + id_or_imm * x の番地に id_t1 を store *)
    let src = reg.(int_of_id_t id_t1) in
    let dest = reg.(int_of_id_t id_t2) in
    let offset = (int_of_id_or_imm id_or_imm) * x in
    let m = dest + offset in
    Logger.debug (Printf.sprintf "St (dest: %d, offset: %d, m: %d), res: %d" dest offset m src);
    mem.(m) <- src;
    0
  | Comment _ -> 0
  | FNegD id_t -> (- int_of_id_t id_t)
  | FMovD id_t -> int_of_id_t id_t
  | FAddD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg.(x') + reg.(y')
  | FSubD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg.(x') - reg.(y')
  | FMulD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg.(x') * reg.(y')
  | FDivD (x, y) ->
    let x' = int_of_id_t x in
    let y' = int_of_id_t x in
    reg.(x') / reg.(y')
  | IfEq (id1, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id1) in
    let r2 = reg.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfEq (id1: %s, id2: %s, t1: %d, t2: %d)" id1 (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 = r2 then
      interp prog t1 reg  mem
    else
      interp prog t2 reg  mem
  | IfLE (id, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id) in
    let r2 = reg.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfLE (id: %s, id_or_imm: %s, t1: %d, t2: %d)" id (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 <= r2 then
      interp prog t1 reg  mem
    else
      interp prog t2 reg  mem
  | IfGE (id, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id) in
    let r2 = reg.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfGE (id1: %s, id2: %s, t1: %d, t2: %d)" id (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 >= r2 then
      interp prog t1 reg  mem
    else
      interp prog t2 reg  mem
  | IfFEq (id1, id2, t1, t2) ->
    let r1 = reg.(int_of_id_t id1) in
    let r2 = reg.(int_of_id_t id2) in
    Logger.debug (Printf.sprintf "IfFEq (id1: %s id2: %s t1: %d, t2: %d)" id1 id2 r1 r2);
    if r1 = r2 then
      interp prog t1 reg  mem
    else
      interp prog t2 reg  mem
  | IfFLE (id1, id2, t1, t2) ->
    let r1 = reg.(int_of_id_t id1) in
    let r2 = reg.(int_of_id_t id2) in
    if r1 <= r2 then
      interp prog t1 reg  mem
    else
      interp prog t2 reg  mem
  | LdDF (id_t, id_or_imm, x) ->
    let m = (int_of_id_t id_t) + (int_of_id_or_imm id_or_imm) * x in
    mem.(m)
  | StDF (id_t1, id_t2, id_or_imm, x) ->
    let src = reg.(int_of_id_t id_t1) in
    let m = (int_of_id_t id_t2) + (int_of_id_or_imm id_or_imm) * x in
    mem.(m) <- src;
    0
  | CallCls (name, args, _) ->
    let addr = reg.(int_of_id_t name) in
    let num = mem.(addr) in
    let ProgInterp (_, _, _, labels) = prog in
    let (id_l, _) =
      try
        List.find (fun (id_l, n) -> n = num) labels
      with Not_found ->
        Logger.error (Printf.sprintf "num: %d" num);
        raise Not_found
    in
    Logger.debug (Printf.sprintf "CallCls (name: %s, mem_num: %d)" name num);
    let fundef = lookup_by_id_l prog id_l in
    let reg' = make_reg reg (fundef.args) args in
    interp prog (fundef.body) reg' mem
  | CallDir (Id.L ("min_caml_print_int"), [arg], _) ->
    let v = reg.(int_of_id_t arg) in
    Logger.debug (Printf.sprintf "CallDir min_caml_print_int %d" v);
    print_int v; 0
  | CallDir (Id.L ("min_caml_print_newline"), _, _) ->
    print_newline (); 0
  | CallDir (Id.L ("min_caml_truncate"), _, [farg]) ->
    reg.(int_of_id_t farg)
  | CallDir (Id.L ("min_caml_create_array"), _, _ ) ->
    raise (Un_implemented_instruction "min_caml_create array is not implemented.")
  | CallDir (name, args, _) ->
    (* fundef.args: 仮引数 args: 実引数 *)
    let fundef = lookup_by_id_l prog name in
    let reg' = make_reg reg (fundef.args) args in
    let Id.L s = name in Logger.debug (Printf.sprintf "CallDir %s" s);
    interp prog (fundef.body) reg'  mem
  | _ -> raise (Un_implemented_instruction "Not implemented.")

let rec create_labels fundefs i =
  match fundefs with
  | [] -> []
  | fundef :: tl -> (fundef.name, i) :: create_labels tl (i + 1)

let to_prog_interp prog =
  let Prog (table, fundefs, exp) = prog in
  let labels = create_labels fundefs 0 in
  ProgInterp (table, fundefs, exp, labels)

let f prog =
  let reg = Array.make register_size 0 in
  let mem = Array.make register_size 0 in
  let prog' = to_prog_interp prog in
  let ProgInterp (_, _, instructions, labels) = prog' in
  ignore (interp prog' instructions reg mem)
