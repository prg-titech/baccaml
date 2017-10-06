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

module Logger = struct
  let info s =
    print_string ("[INFO]" ^ s ^ "\n")

  let debug s =
    print_string ("[DEBUG]" ^ s ^ "\n")
end

module Exception = struct
  exception Un_Implemented_Instruction of string
end

let int_of_id_t (id : Id.t) : int =
  let splitted = Str.split (Str.regexp_string ".") id in
  let num = List.nth splitted 1 in
  int_of_string num

let int_of_id_or_imm = function
  | V (id_t) -> int_of_id_t id_t
  | C (n) -> n

let string_of_id_or_imm = function
  | V (id_t) -> id_t
  | C (n) -> string_of_int n

let rec lookup (prog : prog) (name : Id.l) : fundef  =
  match prog with
  | Prog (_, fundefs, _) ->
    try
      List.find (fun (fundef) -> fundef.name = name) fundefs
    with Not_found ->
      let Id.L s = name in
      print_string (s ^ "\n");
      raise Not_found

let rec lookup_by_id_t (prog : prog) (name : Id.t) : fundef =
  match prog with
  | Prog (_, fundefs, _) ->
    let get_fundef_name = function
      | Id.L (s) -> s
    in
    List.find (fun (fundef) -> (get_fundef_name fundef.name) = name) fundefs

(* 仮引数のレジスタに実引数がしまわれている reg_set を作る *)
let make_reg_set (reg_set : int array) (argst : Id.t list) (argsr : Id.t list) : int array =
  let regst = List.map int_of_id_t argst in
  let regsr = List.map int_of_id_t argsr in
  let rec set = function
    | [] -> ()
    | (x, y) :: tl ->
      let r = reg_set.(y) in
      Printf.printf "[DEBUG] make_reg_set x: %d y: %d r: %d\n" x y r;
      reg_set.(x) <- r; set tl
  in
  set (ListUtil.zip regst regsr);
  reg_set

exception Un_Implemented_Instruction of string

let rec interp (program : prog) (instruction : Asm.t) (reg_set : int array) (mem : int array) : 'a =
  match instruction with
  | Ans exp -> interp' program exp reg_set mem
  | Let ((id, _), exp, body) ->
    let reg_num = int_of_id_t id in
    let res = interp' program exp reg_set mem in
    Logger.debug ("Let " ^ id);
    reg_set.(reg_num) <- res;
    interp program body reg_set mem
and interp' (program : prog) (exp' : exp) (reg_set : int array) (mem : int array) : 'a =
  match exp' with
  | Nop ->
    Logger.debug "Nop ";
    0
  | Set n ->
    Logger.debug ("Set " ^ (string_of_int n));
    n
  | Neg n ->
    Logger.debug ("Neg " ^ n);
    (- int_of_id_t n)
  | SetL (Id.L (s)) ->
    Logger.debug ("SetL " ^ (s));
    int_of_string s
  | Mov id_t ->
    let res = int_of_id_t id_t in
    Logger.debug ("Mov " ^ (id_t));
    res
  | Add (id_t, id_or_imm) ->
    let r1 = int_of_id_t id_t in
    let r2 = int_of_id_or_imm id_or_imm in
    Logger.debug ("Add " ^ (string_of_int r1) ^ " " ^ (string_of_int r2));
    reg_set.(r1) + reg_set.(r2)
  | Sub (id_t, id_or_imm) ->
    let r1 = int_of_id_t id_t in
    let r2 = int_of_id_or_imm id_or_imm in
    Logger.debug ("Sub " ^ (string_of_int r1) ^ " " ^ (string_of_int r2));
    reg_set.(r1) - reg_set.(r2)
  | Ld (id_t, id_or_imm, x) ->
    (* id_t + id_or_imm * x の番地から load *)
    let m = (int_of_id_t id_t) + (int_of_id_or_imm id_or_imm) * x in
    mem.(m)
  | St (id_t1, id_t2, id_or_imm, x) ->
    (* id_t2 + id_or_imm * x の番地に id_t1 を store *)
    let src = int_of_id_t id_t1 in
    let m = (int_of_id_t id_t2) + (int_of_id_or_imm id_or_imm) * x in
    mem.(m) <- src;
    0
  | Comment _ -> 0
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
    Logger.debug ("IfEq " ^ (string_of_int r1) ^ " " ^ (string_of_int r2));
    if r1 = r2 then
      interp program t1 reg_set mem
    else
      interp program t2 reg_set mem
  | IfLE (id, id_or_imm, t1, t2) ->
    Logger.debug ("IfLE " ^ id ^ " " ^ (string_of_id_or_imm id_or_imm));
    let r1 = reg_set.(int_of_id_t id) in
    let r2 = reg_set.(int_of_id_or_imm id_or_imm) in
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
  | CallCls (name, args, _) ->
    let fundef = lookup_by_id_t program name in
    let args' = fundef.args in
    let body' = fundef.body in
    let reg_set' = make_reg_set reg_set args args' in
    interp program body' reg_set' mem
  | CallDir (Id.L ("min_caml_print_int"), [arg], _) ->
    let v1 = reg_set.(int_of_id_t arg) in
    Logger.debug ("CallDir " ^ ("min_caml_print_int" ^ " " ^ arg));
    print_int v1;
    0
  | CallDir (name, args, _) ->
    let fundef = lookup program name in
    (* 仮引数: args' 実引数: args *)
    let args' = fundef.args in
    let body' = fundef.body in
    let reg_set' = make_reg_set reg_set args' args in
    Logger.debug ("CallDir ");
    interp program body' reg_set' mem
  | _ -> raise (Exception.Un_Implemented_Instruction "Not implemented.")

let f (oc : out_channel) (prog : prog) : unit =
  let reg = Array.make 256 0 in
  let mem = Array.make 256 0 in
  let instructions = match prog with
    | Prog (_, _, t) -> t
  in
  let res = interp prog instructions reg mem in
  Printf.fprintf oc "%d" res
