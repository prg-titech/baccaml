open Asm

let to_string_id id =
  match id with
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
  | Type.Unit -> "Unit"
  | Type.Bool -> "Bool"
  | Type.Int -> "Int"
  | Type.Float -> "Float"
  | Type.Fun (hd :: tl, t) ->
     Printf.sprintf "Fun (%s, %s, %s)" (to_string_type hd) (to_string_type_list tl) (to_string_type t)
  | Type.Tuple (hd :: tl) ->
     Printf.sprintf "Tuple (%s, %s)" (to_string_type hd) (to_string_type_list tl)
  | Type.Array (t) ->
     Printf.sprintf "Array (%s)" (to_string_type t)
  | Type.Var (x) ->
     match !x with
     | None -> "Var (ref None)"
     | Some v -> Printf.sprintf "Var (ref Some (%s))" (to_string_type v)

let rec to_string_exp exp =
  match exp with
  | Nop -> "Nop"
  | Set (i) -> Printf.sprintf "Set (%d)" i
  | SetL (l) -> Printf.sprintf "SetL (%s)" (to_string_id l)
  | Mov (x')-> Printf.sprintf "Mov (%s)" x'
  | Neg (x')-> Printf.sprintf "Neg (%s)" x'
  | Add (x', y') -> Printf.sprintf "Add (%s, %s)" x' (to_string_id_or_imm y')
  | Sub (x', y') -> Printf.sprintf "Sub (%s, %s)" x' (to_string_id_or_imm y')
  | Ld (x', y', i) -> Printf.sprintf "Ld (%s, %s %d)" x' (to_string_id_or_imm y') i
  | St (x1, x2, y', i) -> Printf.sprintf "St (%s, %s, %s, %d)" x1 x2 (to_string_id_or_imm y') i
  | FMovD (x') -> Printf.sprintf "FMovd (%s)" x'
  | FNegD (x') -> Printf.sprintf "FNegD (%s)" x'
  | FAddD (x', y') -> Printf.sprintf "FAddD (%s, %s)" x' y'
  | FSubD (x', y') -> Printf.sprintf "FSubD (%s, %s)" x' y'
  | FMulD (x', y') -> Printf.sprintf "FMulD (%s, %s)" x' y'
  | FDivD (x', y') -> Printf.sprintf "FDivD (%s, %s)" x' y'
  | LdDF (x', y', i) ->
     let id_or_imm = to_string_id_or_imm y' in
     Printf.sprintf "LdDF (%s, %s, %d)" x' id_or_imm i
  | StDF (x1, x2, y', i) ->
     let id_or_imm = to_string_id_or_imm y' in
     Printf.sprintf "StDf (%s, %s, %s, %d)" x1 x2 id_or_imm i
  | Comment (s) -> Printf.sprintf "Comment (%s)" s
    
let rec to_string_t t =
  match t with
  | Ans (exp) ->
     Printf.sprintf "Ans (%s)" (to_string_exp exp)
  | Let (x', exp, t') ->
     match x' with
     | (id', type') ->
        Printf.sprintf "Let ((%s, %s), %s, %s)" id' (to_string_type type') (to_string_exp exp) (to_string_t t')

let print_exp exp =
  Printf.printf "%s" (to_string_exp exp)
