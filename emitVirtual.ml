open Asm

let to_string_id id =
  match id with
  | Id.L (s) -> s

let to_string_id_or_imm id_or_imm =
  match id_or_imm with
  | V (t) -> t
  | C (i) -> Pervasives.string_of_int i

let print_exp exp =
  match exp with
  | Nop -> Printf.printf "Nop"
  | Set (i) -> Printf.printf "Set (%d)" i
  | SetL (l) -> Printf.printf "SetL (%s)" (to_string_id l)
  | Mov (t)-> Printf.printf "Mov (%s)" t
  | Neg (t)-> Printf.printf "Neg (%s)" t
  | Add (t, id_or_imm) -> Printf.printf "Add (%s, %s)" t (to_string_id_or_imm id_or_imm)
