open Asm
open Util

exception Un_supported of string

type value = Red of int
           | Green of int

type register = value array
type memory = value array

let int_of_id_t = function (* TODO: レジスタ番号をsringで与える実装に変更 *)
  | "min_caml_hp" -> raise (Un_supported ("int_of_id_t min_caml_hp is not supported."))
  | id ->
    try
      int_of_string (String.after_of id '.')
    with _ ->
      int_of_string (String.after_of id 'u')

let int_of_id_or_imm = function
  | V (id_t) -> int_of_id_t id_t
  | C (n) -> n

let get_variable_name str =
  List.hd (Str.split (Str.regexp_string ".") str)

let rec jit (prog : prog) (instr : t) (reg : register) (mem : memory) (reds : string list) (greens : string list): t =
  match instr with
  | Let ((dest, typ), Add (id_t1, id_or_imm), body) ->
     if List.mem (get_variable_name id_t1) greens then
       let res = match reg.(int_of_id_t id_t1) with
         | Red (i) -> i + (int_of_id_or_imm id_or_imm)
         | Green (i) -> i + (int_of_id_or_imm id_or_imm)
       in
       reg.(int_of_id_t dest) <- Green (res);
       jit prog body reg mem reds greens
     else
       Let ((dest, typ), Add ((id_t1), id_or_imm), jit prog body reg mem reds greens)
  | _ ->
     failwith "Not supported."

