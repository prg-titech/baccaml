open Asm
open Util

exception Not_supported of string

let int_of_id_t = function (* TODO: レジスタ番号をsringで与える実装に変更 *)
  | "min_caml_hp" -> raise (Not_supported ("int_of_id_t min_caml_hp is not supported."))
  | id ->
    try
      int_of_string (String.after_of id '.')
    with _ ->
      int_of_string (String.after_of id 'u')

let int_of_id_or_imm = function
    Asm.V (id_t) -> int_of_id_t id_t
  | Asm.C (n) -> n

let string_of_id_or_imm = function
    Asm.V (id_t) -> id_t
  | Asm.C (n) -> string_of_int n

let rec get_body_by_id_l prog name =
  let Asm.Prog (_, fundefs, _) = prog in
  try
    List.find (fun fundef -> fundef.name = name) fundefs
  with
  | _ -> failwith "Unknown"
