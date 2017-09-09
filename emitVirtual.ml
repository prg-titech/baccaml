open Asm

let to_string_idl idl =
  match idl with
  | Id.L (s) -> s

let to_string_id_or_imm id_or_imm =
  match id_or_imm with
  | V (t) -> t
  | C (i) -> Pervasives.string_of_int i

let rec to_string_type typ =  
  let rec to_string_type_list lst =
    match lst with
    | [] -> ""
    | h :: t -> (to_string_type h) ^ ", " ^ to_string_type_list t
  in
  match typ with
  | Type.Unit -> "Unit\n"
  | Type.Bool -> "Bool\n"
  | Type.Int -> "Int\n"
  | Type.Float -> "Float\n"
  | Type.Fun (hd :: tl, t) ->
     Printf.sprintf "Fun (%s, %s, %s)\n" (to_string_type hd) (to_string_type_list tl) (to_string_type t)
  | Type.Tuple (hd :: tl) ->
     Printf.sprintf "Tuple (%s, %s)\n" (to_string_type hd) (to_string_type_list tl)
  | Type.Array (t) ->
     Printf.sprintf "Array (%s)\n" (to_string_type t)
  | Type.Var (x) ->
     match !x with
     | None -> "Var (ref None)\n"
     | Some v -> Printf.sprintf "Var (ref Some (%s))\n" (to_string_type v)


let rec to_string_ids ids =
  match ids with
  | [] -> ""
  | hd :: tl -> hd ^ (to_string_ids tl)


(* Asm.exp to string *)
let rec to_string_exp exp =
  match exp with
  | Nop -> "Nop\n"
  | Set (i) -> Printf.sprintf "Set (%d)\n" i
  | SetL (l) -> Printf.sprintf "SetL (%s)\n" (to_string_idl l)
  | Mov (x')-> Printf.sprintf "Mov (%s)\n" x'
  | Neg (x')-> Printf.sprintf "Neg (%s)\n" x'
  | Add (x', y') -> Printf.sprintf "Add (%s, %s)\n" x' (to_string_id_or_imm y')
  | Sub (x', y') -> Printf.sprintf "Sub (%s, %s)\n" x' (to_string_id_or_imm y')
  | Ld (x', y', i) -> Printf.sprintf "Ld (%s, %s %d)\n" x' (to_string_id_or_imm y') i
  | St (x1, x2, y', i) -> Printf.sprintf "St (%s, %s, %s, %d)\n" x1 x2 (to_string_id_or_imm y') i
  | FMovD (x') -> Printf.sprintf "FMovd (%s)\n" x'
  | FNegD (x') -> Printf.sprintf "FNegD (%s)\n" x'
  | FAddD (x', y') -> Printf.sprintf "FAddD (%s, %s)\n" x' y'
  | FSubD (x', y') -> Printf.sprintf "FSubD (%s, %s)\n" x' y'
  | FMulD (x', y') -> Printf.sprintf "FMulD (%s, %s)\n" x' y'
  | FDivD (x', y') -> Printf.sprintf "FDivD (%s, %s)\n" x' y'
  | LdDF (x', y', i) ->
     let id_or_imm = to_string_id_or_imm y' in
     Printf.sprintf "LdDF (%s, %s, %d)\n" x' id_or_imm i
  | StDF (x1, x2, y', i) ->
     let id_or_imm = to_string_id_or_imm y' in
     Printf.sprintf "StDf (%s, %s, %s, %d)\n" x1 x2 id_or_imm i
  | Comment (s) -> Printf.sprintf "Comment (%s)\n" s
  (* virtual instructions *)
  | IfEq (x', y', e1, e2) ->
     let t1 = to_string_t e1 in
     let t2 = to_string_t e2 in
     Printf.sprintf "IfEq (%s, %s, %s, %s)\n" x' (to_string_id_or_imm y') t1 t2
  | IfLE (x', y', e1, e2) ->
     let t1 = to_string_t e1 in
     let t2 = to_string_t e2 in
     Printf.sprintf "IfLE (%s, %s, %s, %s)\n" x' (to_string_id_or_imm y') t1 t2
  | IfGE (x', y', e1, e2) ->
     let t1 = to_string_t e1 in
     let t2 = to_string_t e2 in
     Printf.sprintf "IfGE (%s, %s, %s, %s)\n" x' (to_string_id_or_imm y') t1 t2
  | IfFEq (x1, x2, e1, e2) ->
     let t1 = to_string_t e1 in
     let t2 = to_string_t e2 in
     Printf.sprintf "IfFEq (%s, %s, %s, %s)\n" x1 x2 t1 t2
  | IfFLE (x1, x2, e1, e2) ->
     let t1 = to_string_t e1 in
     let t2 = to_string_t e2 in
     Printf.sprintf "IfFLE (%s, %s, %s, %s)\n" x1 x2 t1 t2
  | CallCls (x', ids1, ids2) ->
     Printf.sprintf "CallCls (%s, %s, %s)\n" x' (to_string_ids ids1) (to_string_ids ids2)
  | CallDir (l', ids1, ids2) ->
     Printf.sprintf "CallCls (%s, %s, %s)\n" (to_string_idl l') (to_string_ids ids1) (to_string_ids ids2)
  | Save (x1, x2) ->
     Printf.sprintf "Save (%s, %s)\n" x1 x2
  | Restore x ->
     Printf.sprintf "Restore (%s)\n" x

(* Asm.t to string *)
and to_string_t t =
  match t with
  | Ans (exp) ->
     Printf.sprintf "Ans (%s)\n" (to_string_exp exp)
  | Let (x', exp, t') ->
     match x' with
     | (id', type') ->
        Printf.sprintf "Let ((%s, %s), %s, %s)\n" id' (to_string_type type') (to_string_exp exp) (to_string_t t')

(* fundef to string *)
let to_string_fundef fundef' =
  match fundef' with
  | { name = n; args = a; fargs = f; body = b; ret = r } ->
     let name_str = to_string_idl n in
     let args_str = to_string_ids a in
     let fargs_str = to_string_ids f in
     let body_str = to_string_t b in
     let ret_str = to_string_type r in
     Printf.sprintf "{name = %s; args = %s; fargs = %s; body = %s; ret = %s}\n" name_str args_str fargs_str body_str ret_str

let rec to_string_floating_point_table lst =
  match lst with
  | [] -> ""
  | hd :: tl ->
     let (idl, f) = hd in
     (to_string_idl idl) ^ (Pervasives.string_of_float f) ^ (to_string_floating_point_table lst)

(* Asm.prog to string *)
let to_string_prog p =
  match p with
  | Prog (xs, fundefs, t') ->
     let table = to_string_floating_point_table xs in
     let toplevel_function = String.concat " " (List.map to_string_fundef fundefs) in
     let main_exp = to_string_t t' in
     Printf.sprintf "Prog (%s, %s, %s)\n" table toplevel_function main_exp

(* entry point *)
let f asm_prog =
  Printf.printf "%s" (to_string_prog asm_prog)
