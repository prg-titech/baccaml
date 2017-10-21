open Asm
open Util

exception Un_implemented_instruction of string

type labels = (Id.l * int) list (* function label for closures *)
type prog_with_label = ProgWithLabel of (Id.l * float) list * fundef list * t * labels (* prog for interpreter *)
type register = int array
type memory = int array

let register_size = 1000
let heap_pointer = ref 0

let int_of_id_t = function (* TODO: レジスタ番号をsringで与える実装に変更 *)
  | "min_caml_hp" -> !heap_pointer
  | id ->
    try
      int_of_string (String.split id '.')
    with _ ->
      int_of_string (String.split id 'u')

let int_of_id_or_imm = function V (id_t) -> int_of_id_t id_t | C (n) -> n

let string_of_id_or_imm = function V (id_t) -> id_t | C (n) -> string_of_int n

let rec find_label_number label = function
  | [] -> let Id.L s = label in int_of_id_t s
  | (l, num) :: tl -> if l = label then num else find_label_number label tl

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

let make_reg reg args_tmp args_real = (* 仮引数のレジスタに実引数がしまわれている reg を作る *)
  let regs_tmp = List.map int_of_id_t args_tmp in
  let regs_real = List.map int_of_id_t args_real in
  let arr = Array.make 10000 0 in
  ignore (
    List.map
      (fun (x, y) -> arr.(x) <- reg.(y))
      (List.zip regs_tmp regs_real));
  arr

let rec interp (prog : prog_with_label) (instruction : Asm.t) (reg : register) (mem : memory) : 'a =
  match instruction with
  | Ans exp ->
    let res = interp' prog exp reg mem in
    Logger.debug (Printf.sprintf "Ans (%d)" res);
    res
  | Let ((id, _), CallDir (Id.L ("min_caml_create_array"), arg1 :: arg2 :: [], _), body) ->
    let reg_num = int_of_id_t id in
    let size = reg.(int_of_id_t arg1) in
    let init = reg.(int_of_id_t arg2) in
    Logger.debug (Printf.sprintf "Let (id: %s, reg_num: %d, min_caml_create_array, size: %d, init: %d)" id reg_num size init);
    for i = 0 to (size - 1) * 4 do
      mem.(reg_num + i) <- init
    done;
    interp prog body reg mem
  | Let ((id, _), exp, body) ->
    let reg_num = int_of_id_t id in
    let res = interp' prog exp reg mem in
    Logger.debug(Printf.sprintf "Let (id: %s, reg_num: %d, res: %d)" id reg_num res);
    if id = "min_caml_hp" then heap_pointer := res else reg.(reg_num) <- res;
    interp prog body reg  mem

and interp' (prog : prog_with_label) (exp' : exp) (reg : register) (mem : memory) : 'a =
  match exp' with
  | Nop ->
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
    Logger.debug (Printf.sprintf "Mov (min_caml_hp: %d)" !heap_pointer);
    !heap_pointer
  | Mov id_t ->
    let regnum = int_of_id_t id_t in
    let res = reg.(regnum) in
    Logger.debug (Printf.sprintf "Mov (id_t: %s, regnum: %d, res: %d)" id_t regnum res);
    res
  | Add (id_t1, V (id_t2)) ->
    (match id_t1 with
     | "min_caml_hp" ->
       let r1 = !heap_pointer in
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
       let r1 = !heap_pointer in
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
       let r1 = !heap_pointer in
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
       let r1 = !heap_pointer in
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
      | "min_caml_hp" -> !heap_pointer
      | _ -> int_of_id_t id_t
    in
    let offset = (match id_or_imm with
        | V id_t -> reg.(int_of_id_t id_t)
        | C n -> n) * x
    in
    let res = mem.(dest + offset) in
    Logger.debug (Printf.sprintf "Ld (dest: %d, offset: %d, res: %d)" dest offset res);
    res
  | St (id_t1, id_t2, id_or_imm, x) ->
    (* id_t2 + id_or_imm * x の番地に id_t1 を store *)
    let src =  match id_t1 with
        "min_caml_hp" -> !heap_pointer
      | _ -> reg.(int_of_id_t id_t1)
    in
    let dest = match id_t1 with
        "min_caml_hp" -> !heap_pointer
      | _ -> int_of_id_t id_t2
    in
    let offset = (match id_or_imm with
        | V "min_caml_hp" -> !heap_pointer
        | V id_t -> reg.(int_of_id_t id_t)
        | C n -> n) * x
    in
    let m = dest + offset in
    Logger.debug (Printf.sprintf "St (id_t1: %s, id_t2: %s, dest: %d, offset: %d, m: %d), res: %d" id_t1 id_t2 dest offset m src);
    mem.(m) <- src;
    0
  | Comment _ -> 0
  | IfEq (id1, id_or_imm, t1, t2) ->
    let r1 = match id1 with "min_caml_hp" -> !heap_pointer | _ -> reg.(int_of_id_t id1) in
    let r2 = reg.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfEq (id1: %s, id2: %s, t1: %d, t2: %d)" id1 (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 = r2 then
      interp prog t1 reg  mem
    else
      interp prog t2 reg  mem
  | IfLE (id, id_or_imm, t1, t2) ->
    let r1 = match id with "min_caml_hp" -> !heap_pointer | _ -> reg.(int_of_id_t id) in
    let r2 = reg.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfLE (id: %s, id_or_imm: %s, t1: %d, t2: %d)" id (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 <= r2 then
      interp prog t1 reg  mem
    else
      interp prog t2 reg  mem
  | IfGE (id, id_or_imm, t1, t2) ->
    let r1 = match id with "min_caml_hp" -> !heap_pointer | _ -> reg.(int_of_id_t id) in
    let r2 = reg.(int_of_id_or_imm id_or_imm) in
    Logger.debug (Printf.sprintf "IfGE (id1: %s, id2: %s, t1: %d, t2: %d)" id (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 >= r2 then
      interp prog t1 reg  mem
    else
      interp prog t2 reg  mem
  | CallCls (name, args, _) ->
    let address = reg.(int_of_id_t name) in
    let num = mem.(address) in
    let ProgWithLabel (_, _, _, labels) = prog in
    let (id_l, _) =
      try
        List.find (fun (id_l, n) -> n = num) labels
      with Not_found ->
        Logger.error (Printf.sprintf "num: %d" num);
        raise Not_found
    in
    Logger.debug (Printf.sprintf "CallCls (name: %s, address: %d, mem_num: %d)" name address num);
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
    raise (Un_implemented_instruction "min_caml_truncate is not implemented.")
  | CallDir (name, args, _) ->
    (* fundef.args: 仮引数 args: 実引数 *)
    let fundef = lookup_by_id_l prog name in
    let reg' = make_reg reg (fundef.args) args in
    let Id.L s = name in Logger.debug (Printf.sprintf "CallDir %s" s);
    interp prog (fundef.body) reg'  mem
  | _ -> raise (Un_implemented_instruction "Not implemented.")

let to_prog_with_label prog =
  let rec create_labels fundefs i =
    match fundefs with
    | [] -> []
    | fundef :: tl -> (fundef.name, i) :: create_labels tl (i + 1)
  in
  let Prog (table, fundefs, exp) = prog in
  let labels = create_labels fundefs 0 in
  ProgWithLabel (table, fundefs, exp, labels)

let f prog =
  let reg = Array.make register_size 0 in
  let mem = Array.make register_size 0 in
  let prog' = to_prog_with_label prog in
  let ProgWithLabel (_, _, instructions, labels) = prog' in
  ignore (interp prog' instructions reg mem)
