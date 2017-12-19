open Core

open Asm

exception Not_supported of string

type value =
  | Red of int
  | Green of int

type jit_result =
  | Specialized of value
  | Not_specialised of exp

type jit_branch_result =
  | Selected of t
  | Not_selected of exp

type jit_args =
  { trace_name : string
  ; reds : string list
  ; greens: string list
  ; loop_header : int
  ; loop_pc : int
  }

let enable_jit = ref false

let value_of = function
  | Red (n) -> n
  | Green (n) -> n

let int_of_id_t = function (* TODO: レジスタ番号をsringで与える実装に変更 *)
  | "min_caml_hp" -> raise (Not_supported ("int_of_id_t min_caml_hp is not supported."))
  | id ->
    match List.last (String.split id ~on:'.') with
    | Some v -> int_of_string v
    | None -> int_of_string id

let int_of_id_or_imm = function
    Asm.V (id_t) -> int_of_id_t id_t
  | Asm.C (n) -> n

let string_of_id_or_imm = function
    Asm.V (id_t) -> id_t
  | Asm.C (n) -> string_of_int n

let rec get_body_by_id_l prog name =
  let Asm.Prog (_, fundefs, _) = prog in
  try
    List.find_exn fundefs ~f:(fun fundef -> fundef.name = name)
  with
  | _ -> failwith "Unknown"
