open Asm
open Core

type value =
  | Red of int
  | Green of int

type jit_result =
  | Specialized of value
  | Not_specialised of exp * value

let not_specialised exp = Not_specialised (exp, Red (-99))

type jit_branch_result =
  | Selected of t
  | Not_selected of exp

type jit_args =
  { trace_name : string
  ; reds : string list
  ; greens: string list
  ; loop_header : int
  ; loop_pc_place : int
  }

let enable_jit = ref false

let value_of = function
  | Red (n) -> n
  | Green (n) -> n

let is_red = function
  | Red _ -> true
  | Green _ -> false

let is_green = function
  | Red _ -> false
  | Green _ -> true

let int_of_id_t = function (* TODO: レジスタ番号をsringで与える実装に変更 *)
  | "min_caml_hp" -> failwith ("int_of_id_t min_caml_hp is not supported.")
  | id ->
    match List.last (String.split id ~on:'.') with
    | Some v -> int_of_string v
    | None -> int_of_string id

let rec find_fundef prog name =
  let Asm.Prog (_, fundefs, _) = prog in
  match List.find fundefs ~f:(fun fundef -> fundef.name = name) with
  | Some (body) -> body
  | None -> failwith "find_fundef is failed"

let find_pc args jit_args =
  match List.nth args (jit_args.loop_pc_place) with
  | Some (s) -> int_of_id_t s
  | None -> failwith "find_pc is failed"

let find_a args n =
  match List.nth args n with
  | Some (s) -> int_of_id_t s
  | None -> failwith "find_pc is failed"
