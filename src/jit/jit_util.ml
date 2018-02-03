open Asm
open Core

let int_of_id_t = function (* TODO: レジスタ番号をsringで与える実装に変更 *)
  | "min_caml_hp" -> failwith ("int_of_id_t min_caml_hp is not supported.")
  | id ->
    match List.last (String.split id ~on:'.') with
    | Some (str) -> int_of_string str
    | None -> int_of_string (List.last_exn (String.split id ~on:'u'))

let string_of_id_or_imm = function
    Asm.V (id_t) -> id_t
  | Asm.C (n) -> string_of_int n
