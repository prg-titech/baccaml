open MinCaml
open Asm

let zero = "zero.0"

type value = Red of int | Green of int | LightGreen of int

type reg = value array

type mem = value array

type jit_result =
  | Specialized of value
  | Not_specialized of exp * value

let int_of_id_t id =
  if id = "min_caml_hp" then
    failwith ("int_of_id_t min_caml_hp is not supported.")
  else
    try
      let x = String.split_on_char '.' id  in
      List.nth x (List.length x - 1) |> int_of_string
    with
    | _ ->
      try
        let y = String.split_on_char 'u' id  in
        List.nth y (List.length y - 1) |> int_of_string
      with
      | _ -> failwith @@ Printf.sprintf "int_of_id_t is failed: %s" id

let value_of = function
  | Red (n) | Green (n) | LightGreen (n) -> n

let is_red = function
  | Red _ -> true
  | _ -> false

let is_green = function
  | Green _ -> true
  | _ -> false

let is_light_green = function
  | LightGreen _ -> true
  | _ -> false

let string_of_id_or_imm = function
    V (id_t) -> id_t
  | C (n) -> string_of_int n

let string_of_id_l = function
  | Id.L (x) -> x

let string_of_value = function
  | Green (n) -> Printf.sprintf "Green (%d)" n
  | LightGreen (n) -> Printf.sprintf "LightGreen (%d)" n
  | Red (n) -> Printf.sprintf "Red (%d)" n

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with _ -> false

let rec find_fundef prog name =
  let Asm.Prog (_, fundefs, _) = prog in
  match List.find_opt (fun fundef -> fundef.name = name) fundefs with
  | Some (body) -> body
  | None -> failwith "find_fundef in Method jit is failed"

let jit_value_of_id_t reg id_t = reg.(int_of_id_t id_t)

let jit_value_of_id_or_imm reg = function
  | V (id) -> reg.(int_of_id_t id)
  | C (n) -> Green (n)

let name_of id =
  try List.hd (String.split_on_char '.' id) with
  | _ -> id

let is_opcode id =
  contains (name_of id) "instr"

let _ =
  assert (is_opcode "instr")

let rec connect id_t instr body =
  let rec go id_t instr body = match instr with
    | Let (a, Nop, t) -> go id_t t body
    | Let (a, e, t) -> Let (a, e, go id_t t body)
    | Ans Nop -> body
    | Ans e -> Let ((id_t, Type.Int), e, body)
  in go id_t instr body
