open Asm

let to_string_idl idl =
  match idl with
  | Id.L (s) -> "\"" ^ s ^ "\""

let to_string_id_or_imm id_or_imm =
  match id_or_imm with
  | V (t) -> Printf.sprintf "\"%s\"" t
  | C (i) -> Printf.sprintf "%d" i

let rec to_string_type_list lst =
  let rec loop = function
    | [] -> ""
    | hd :: tl -> (to_string_type hd) ^ "; " ^ (to_string_type_list tl)
  in "[" ^ (loop lst) ^ "]"

and to_string_type typ =
  match typ with
  | Type.Unit -> "Unit"
  | Type.Bool -> "Bool"
  | Type.Int -> "Int"
  | Type.Float -> "Float"
  | Type.Fun ([], _) -> Printf.sprintf "Fun ()"
  | Type.Fun (hd :: tl, t) ->
    Printf.sprintf "Fun (%s, %s, %s)" (to_string_type hd) (to_string_type_list tl) (to_string_type t)
  | Type.Tuple ([]) ->
    Printf.sprintf "Tuple ()"
  | Type.Tuple (hd :: tl) ->
    Printf.sprintf "Tuple (%s, %s)" (to_string_type hd) (to_string_type_list tl)
  | Type.Array (t) ->
    Printf.sprintf "Array (%s)" (to_string_type t)
  | Type.Var (x) ->
    match !x with
    | None -> "Var (ref None)"
    | Some v -> Printf.sprintf "Var (ref Some (%s))" (to_string_type v)


let rec to_string_ids ids =
  match ids with
  | [] -> "[]"
  | _ -> "[" ^ (String.concat "; " ids) ^ "]"


(* Asm.exp to string *)
let rec to_string_exp exp =
  match exp with
  | Nop -> "Nop"
  | Set (i) -> Printf.sprintf "Set (%d)" i
  | SetL (l) -> Printf.sprintf "SetL (Id.L (\"%s\"))" (to_string_idl l)
  | Mov (x')-> Printf.sprintf "Mov (\"%s\")" x'
  | Neg (x')-> Printf.sprintf "Neg (%s)" x'
  | Add (x', y') -> Printf.sprintf "Add (\"%s\", %s)" x' (to_string_id_or_imm y')
  | Sub (x', y') -> Printf.sprintf "Sub (\"%s\", %s)" x' (to_string_id_or_imm y')
  | Ld (x', y', i) -> Printf.sprintf "Ld (\"%s\", %s, %d)" x' (to_string_id_or_imm y') i
  | St (x1, x2, y', i) -> Printf.sprintf "St (\"%s\", \"%s\", %s, %d)" x1 x2 (to_string_id_or_imm y') i
  | FMovD (x') -> Printf.sprintf "FMovd (%s)" x'
  | FNegD (x') -> Printf.sprintf "FNegD (%s)" x'
  | FAddD (x', y') -> Printf.sprintf "FAddD (\"%s\", %s)" x' y'
  | FSubD (x', y') -> Printf.sprintf "FSubD (\"%s\", %s)" x' y'
  | FMulD (x', y') -> Printf.sprintf "FMulD (\"%s\", %s)" x' y'
  | FDivD (x', y') -> Printf.sprintf "FDivD (\"%s\", %s)" x' y'
  | LdDF (x', y', i) ->
    let id_or_imm = to_string_id_or_imm y' in
    Printf.sprintf "LdDF (\"%s\", %s, %d)" x' id_or_imm i
  | StDF (x1, x2, y', i) ->
    let id_or_imm = to_string_id_or_imm y' in
    Printf.sprintf "StDf (\"%s\", %s, %s, %d)" x1 x2 id_or_imm i
  | Comment (s) -> Printf.sprintf "Comment (%s)" s
  (* virtual instructions *)
  | IfEq (x', y', e1, e2) ->
    let t1 = to_string_t e1 in
    let t2 = to_string_t e2 in
    Printf.sprintf "IfEq (\"%s\", %s, %s, %s)" x' (to_string_id_or_imm y') t1 t2
  | IfLE (x', y', e1, e2) ->
    let t1 = to_string_t e1 in
    let t2 = to_string_t e2 in
    Printf.sprintf "IfLE (\"%s\", %s, %s, %s)" x' (to_string_id_or_imm y') t1 t2
  | IfGE (x', y', e1, e2) ->
    let t1 = to_string_t e1 in
    let t2 = to_string_t e2 in
    Printf.sprintf "IfGE (\"%s\", %s, %s, %s)" x' (to_string_id_or_imm y') t1 t2
  | IfFEq (x1, x2, e1, e2) ->
    let t1 = to_string_t e1 in
    let t2 = to_string_t e2 in
    Printf.sprintf "IfFEq (\"%s\", %s, %s, %s)" x1 x2 t1 t2
  | IfFLE (x1, x2, e1, e2) ->
    let t1 = to_string_t e1 in
    let t2 = to_string_t e2 in
    Printf.sprintf "IfFLE (\"%s\", %s, %s, %s)" x1 x2 t1 t2
  | CallCls (x', ids1, ids2) ->
    Printf.sprintf "CallCls (\"%s\", %s, %s)" x' (to_string_ids ids1) (to_string_ids ids2)
  | CallDir (l', ids1, ids2) ->
    Printf.sprintf "CallDir (Id.L (%s), %s, %s)" (to_string_idl l') (to_string_ids ids1) (to_string_ids ids2)
  | Save (x1, x2) ->
    Printf.sprintf "Save (\"%s\", \"%s\")" x1 x2
  | Restore x ->
    Printf.sprintf "Restore (\"%s\")" x

(* Asm.t to string *)
and to_string_t t =
  match t with
  | Ans (exp) ->
    Printf.sprintf "\n  Ans (%s)" (to_string_exp exp)
  | Let (x', exp, t') ->
    match x' with
    | (id', type') ->
      Printf.sprintf "\n  Let ((\"%s\", %s), %s, %s)" id' (to_string_type type') (to_string_exp exp) (to_string_t t')

(* fundef to string *)
let to_string_fundef fundef' =
  match fundef' with
  | { name = n; args = a; fargs = f; body = b; ret = r } ->
    let name_str = to_string_idl n in
    let args_str = to_string_ids a in
    let fargs_str = to_string_ids f in
    let body_str = to_string_t b in
    let ret_str = to_string_type r in
    Printf.sprintf "\n{ name = %s; args = %s; fargs = %s; body = %s; ret = %s }" name_str args_str fargs_str body_str ret_str

let rec to_string_floating_point_table lst =
  let rec loop lst res =
    match lst with
    | [] -> "[" ^ res ^ "]"
    | hd :: tl ->
      let (idl, f) = hd in
      let res' = (to_string_idl idl) ^ ", " ^ (Pervasives.string_of_float f) ^  ", " ^ res in
      loop tl res'
  in
  loop lst ""

(* Asm.prog to string *)
let to_string_prog p =
  match p with
  | Prog (xs, fundefs, t') ->
    let xs' = to_string_floating_point_table xs in
    let fundefs' = "[" ^ String.concat "; " (List.map to_string_fundef fundefs) ^ "]" in
    let main_exp = to_string_t t' in
    Printf.sprintf "Prog (%s, %s, %s)" xs' fundefs' main_exp

let to_string_labels labels =
  List.map (fun (id_l, i) -> Printf.sprintf "Id.L (%s), %d" (let Id.L s = id_l in s) i) labels

(* Asm.prog -> Interp.ProgInterp *)
let g oc asm_prog =
  let Interp.ProgWithLabel (xs, fundefs, t', labels) = Interp.to_prog_with_label asm_prog in
  let xs' = to_string_floating_point_table xs in
  let fundefs' = "[" ^ String.concat ", " (List.map to_string_fundef fundefs) ^ "]" in
  let main_exp = to_string_t t' in
  let labels' = "(" ^ String.concat "; " (to_string_labels labels) ^ ")" in
  Printf.fprintf oc "ProgInterp (table = %s, fundefs = %s,\n main_exp = %s,\n labels = %s)" xs' fundefs' main_exp labels'

(* entry point *)
let f oc asm_prog =
  Printf.fprintf oc "%s" (to_string_prog asm_prog)
