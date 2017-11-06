open Asm
open Util

exception Un_implemented_instruction of string
exception Un_supported of string

type labels = (Id.l * int) list (* function label for closures *)
type prog_with_label = ProgWithLabel of (Id.l * float) list * fundef list * t * labels (* prog for interpreter *)
type register = int array
type memory = int array

let register_size = 128
let heap = ref 0


let int_of_id_t = function (* TODO: レジスタ番号をsringで与える実装に変更 *)
  | "min_caml_hp" -> raise (Un_supported ("int_of_id_t min_caml_hp is not supported."))
  | id ->
    try
      int_of_string (String.after_of id '.')
    with _ ->
      int_of_string (String.after_of id 'u')


let int_of_id_or_imm = function
    V (id_t) -> int_of_id_t id_t
  | C (n) -> n


let string_of_id_or_imm = function
    V (id_t) -> id_t
  | C (n) -> string_of_int n


let rec find_label_number label = function
  | [] -> let Id.L s = label in int_of_id_t s
  | (l, num) :: tl -> if l = label then num else find_label_number label tl


let rec find_label prog num =
  let ProgWithLabel (_, _, _, labels) = prog in
  try
    let (id, _) = (List.find (fun (id_l, n) -> n = num) labels) in id
  with
    Not_found ->
    Logger.error (Printf.sprintf "num: %d" num);
    raise Not_found


let rec lookup_by_id_l prog name =
  let ProgWithLabel (_, fundefs, _, _) = prog in
  try
    List.find (fun fundef -> (fundef.name = name)) fundefs
  with e ->
    Logger.error (let Id.L s = name in Printf.sprintf "CallCls %s" s); raise e


let rec lookup_by_id_t prog name =
  let ProgWithLabel (_, fundefs, _, _) = prog in
  try
    List.find (fun fundef -> (let Id.L s = fundef.name in s) = name) fundefs
  with e ->
    Logger.error (Printf.sprintf "CallCls %s" name); raise e


let to_prog_with_label prog =
  let rec create_labels fundefs i =
    match fundefs with
    | [] -> []
    | fundef :: tl -> (fundef.name, i) :: create_labels tl (i + 1)
  in
  let Prog (table, fundefs, exp) = prog in
  let labels = create_labels fundefs 0 in
  ProgWithLabel (table, fundefs, exp, labels)


let make_reg reg args_tmp args_real = (* 仮引数のレジスタに実引数がしまわれている reg を作る *)
  let regs_tmp = List.map int_of_id_t args_tmp in
  let regs_real = List.map int_of_id_t args_real in
  let arr = Array.make register_size 0 in
  List.iter
    (fun (x, y) ->
       Logger.debug (Printf.sprintf "make_reg (tmp_address: %d <- real_address: %d, value: %d)" x y reg.(y));
       arr.(x) <- reg.(y))
    (List.zip regs_tmp regs_real);
  arr


let rec interp (prog : prog_with_label) (instr : Asm.t) (reg : register) (mem : memory) : 'a =
  match instr with
  | Ans exp ->
    let res = eval_exp prog exp reg mem in
    Logger.debug (Printf.sprintf "Ans (%d)" res);
    res
  | Let ((id, _), exp, t) ->
    if id = "min_caml_hp" then
      let res = eval_exp prog exp reg mem in
      Logger.debug(Printf.sprintf "Let (id: %s, reg_num: %d, res: %d)" id !heap res);
      heap := res;
      interp prog t reg mem
    else
      let reg_num = int_of_id_t id in
      let res = eval_exp prog exp reg mem in
      Logger.debug(Printf.sprintf "Let (id: %s, reg_num: %d, res: %d)" id reg_num res);
      reg.(reg_num) <- res;
      interp prog t reg mem


and eval_exp (prog : prog_with_label) (exp' : exp) (reg : register) (mem : memory) : 'a =
  match exp' with
  | Nop ->
    Logger.debug ("Nop");
    0
  | Set n ->
    Logger.debug (Printf.sprintf "Set (%d)" n);
    n
  | Neg n ->
    let res = reg.(int_of_id_t n) in
    Logger.debug (Printf.sprintf "Neg %d" res);
    (- res)
  | SetL id_l ->
    let ProgWithLabel (_, _, _, labels) = prog in
    let res = find_label_number id_l labels in
    Logger.debug (Printf.sprintf "SetL (%s: %d)" (let Id.L s = id_l in s) res);
    res
  | Mov "min_caml_hp" ->
    Logger.debug (Printf.sprintf "Mov (min_caml_hp: %d)" !heap);
    !heap
  | Mov id_t ->
    let regnum = int_of_id_t id_t in
    let res = reg.(regnum) in
    Logger.debug (Printf.sprintf "Mov (id_t: %s, regnum: %d, res: %d)" id_t regnum res);
    res
  | Add (id_t1, V (id_t2)) ->
    (match id_t1 with
     | "min_caml_hp" ->
       let r1 = !heap in
       let r2 = int_of_id_t id_t2 in
       let res = r1 + reg.(r2) in
       Logger.debug (Printf.sprintf "Add (r1: %d, r2: %d, res: %d)" r1 r2 res);
       res
     | _ ->
       let r1 = int_of_id_t id_t1 in
       let r2 = int_of_id_t id_t2 in
       let res = reg.(r1) + reg.(r2) in
       Logger.debug (Printf.sprintf "Add (r1: %d, r2: %d, res: %d)" r1 r2 res);
       res)
  | Add (id_t, C n) ->
    (match id_t with
     | "min_caml_hp" ->
       let r1 = !heap in
       let res = r1 + n in
       Logger.debug (Printf.sprintf "AddImm (r1: %d, r2: %d, res: %d)" r1 n res);
       res
     | _ ->
       let r1 = int_of_id_t id_t in
       let res = reg.(r1) + n in
       Logger.debug (Printf.sprintf "AddImm (r1: %d, r2: %d, res: %d)" r1 n res);
       res)
  | Sub (id_t1, V (id_t2)) ->
    (match id_t1 with
       "min_caml_hp" ->
       let r1 = !heap in
       let r2 = reg.(int_of_id_t id_t2) in
       let res = r1 - r2 in
       Logger.debug (Printf.sprintf "Sub (r1: %d, r2: %d, res: %d)" r1 r2 res);
       res
     | _ ->
       let r1 = int_of_id_t id_t1 in
       let r2 = int_of_id_t id_t2 in
       let res = reg.(r1) - reg.(r2) in
       Logger.debug (Printf.sprintf "Sub (r1: %d, r2: %d, res: %d)" r1 r2 res);
       res)
  | Sub (id_t, C (n)) ->
    (match id_t with
       "min_caml_hp" ->
       let r1 = !heap in
       let res = r1 - n in
       Logger.debug (Printf.sprintf "SubImm (r1: %d, r2: %d, res: %d)" r1 n res);
       res
     | _ ->
       let r1 = int_of_id_t id_t in
       let res = reg.(r1) - n in
       Logger.debug (Printf.sprintf "SubImm (r1: %d, r2: %d, res: %d)" r1 n res);
       res)
  | Ld (id_t, id_or_imm, x) ->
    (* id_t + id_or_imm * x の番地から load *)
    let dest = match id_t with
      | "min_caml_hp" -> !heap
      | _ -> reg.(int_of_id_t id_t)
    in
    let offset = (match id_or_imm with
        | V id_t -> reg.(int_of_id_t id_t)
        | C n -> n) * x
    in
    let res = mem.(dest + offset) in
    Logger.debug (Printf.sprintf "Ld (id_t: %s, dest: %d, offset: %d, m: %d, res: %d)" id_t dest offset (dest + offset) res);
    res
  | St (id_t1, id_t2, id_or_imm, x) ->
    (* id_t2 + id_or_imm * x の番地に id_t1 を store *)
    let src =  match id_t1 with
        "min_caml_hp" -> !heap
      | _ -> reg.(int_of_id_t id_t1)
    in
    let dest = match id_t1 with
        "min_caml_hp" -> !heap
      | _ -> reg.(int_of_id_t id_t2)
    in
    let offset = (match id_or_imm with
        | V "min_caml_hp" -> !heap
        | V id_t -> reg.(int_of_id_t id_t)
        | C n -> n) * x
    in
    let m = dest + offset in
    Logger.debug (Printf.sprintf "St (id_t1: %s, id_t2: %s, dest: %d, offset: %d, m: %d), res: %d" id_t1 id_t2 dest offset m src);
    mem.(m) <- src;
    0
  | Comment _ -> 0
  | IfEq (id1, id_or_imm, t1, t2) ->
    let r1 = match id1 with "min_caml_hp" -> !heap | _ -> reg.(int_of_id_t id1) in
    let r2 = reg.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfEq (id1: %s, id2: %s, t1: %d, t2: %d)" id1 (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 = r2 then
      interp prog t1 reg  mem
    else
      interp prog t2 reg  mem
  | IfLE (id, id_or_imm, t1, t2) ->
    let r1 = match id with "min_caml_hp" -> !heap | _ -> reg.(int_of_id_t id) in
    let r2 = reg.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfLE (id: %s, id_or_imm: %s, t1: %d, t2: %d)" id (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 <= r2 then
      interp prog t1 reg  mem
    else
      interp prog t2 reg  mem
  | IfGE (id, id_or_imm, t1, t2) ->
    let r1 = match id with "min_caml_hp" -> !heap | _ -> reg.(int_of_id_t id) in
    let r2 = reg.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfGE (id1: %s, id2: %s, t1: %d, t2: %d)" id (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 >= r2 then
      interp prog t1 reg  mem
    else
      interp prog t2 reg  mem
  | CallCls (id_t, args, _) ->
    let r1 = reg.(int_of_id_t id_t) in
    let m1 = mem.(r1) in
    let Id.L id = find_label prog m1 in
    Logger.debug (Printf.sprintf "CallCls (id_t: %s, r1: %d, r2: %d, id_l: %s)" id_t r1 m1 id);
    let fundef = lookup_by_id_l prog (Id.L (id)) in
    let reg' = make_reg reg (fundef.args) args in
    reg'.(int_of_id_t id) <- r1;
    interp prog (fundef.body) reg' mem
  | CallDir (Id.L ("min_caml_print_int"), [arg], _) ->
    let v = reg.(int_of_id_t arg) in
    Logger.debug (Printf.sprintf "CallDir min_caml_print_int %d" v);
    print_int v; 0
  | CallDir (Id.L ("min_caml_print_newline"), _, _) ->
    print_newline (); 0
  | CallDir (Id.L ("min_caml_truncate"), _, [farg]) ->
    raise (Un_implemented_instruction "min_caml_truncate is not implemented.")
  | CallDir (Id.L ("min_caml_create_array"), arg1 :: arg2 :: [], _) ->
    let size = reg.(int_of_id_t arg1) in
    let init = reg.(int_of_id_t arg2) in
    let a = !heap in
    heap := !heap + (size * 4);
    for i = 0 to size do
      mem.(a + i * 4) <- init
    done;
    Logger.debug (Printf.sprintf "CallDir (min_caml_create_array, arg1: %d, arg2: %d)" size init);
    a
  | CallDir (name, args, _) ->
    (* fundef.args: 仮引数 args: 実引数 *)
    let fundef = lookup_by_id_l prog name in
    let reg' = make_reg reg (fundef.args) args in
    let Id.L s = name in Logger.debug (Printf.sprintf "CallDir %s" s);
    interp prog (fundef.body) reg'  mem
  | _ -> raise (Un_implemented_instruction "Not implemented.")


let f prog =
  let reg = Array.make register_size 0 in
  let mem = Array.make register_size 0 in
  let prog' = to_prog_with_label prog in
  let ProgWithLabel (_, _, instructions, labels) = prog' in
  ignore (interp prog' instructions reg mem);
  match !Logger.log_level with
  | Logger.Debug ->
    print_string "reg: "; List.print_list print_int (Array.to_list reg);
    print_newline ();
    print_string "mem: "; List.print_list print_int (Array.to_list mem);
  | _ ->
    ()
