open Mincaml
open Asm
open Core

type value =
  | Red of int
  | Green of int
  | LightGreen of int

type reg = value array

type mem = value array

type jit_result =
  | Specialized of value
  | Not_specialized of exp * value

type jit_args =
  { trace_name : string
  ; reds : string list
  ; greens: string list
  ; loop_header : int
  ; loop_pc_place : int
  }

type method_jit_args =
  { method_name : string
  ; reds : string list
  ; method_start : int
  ; method_end : int
  ; pc_place : int
  }

let dummy_jit_args =
  { trace_name = "dummy_jit_args.1000"
  ; reds = []
  ; greens = []
  ; loop_header = 0
  ; loop_pc_place = 0
  }

let enable_jit = ref false

let zero = "zero.0"

let value_of = function
  | Red (n) -> n
  | Green (n) -> n
  | LightGreen (n) -> n

let is_red = function
  | Red _ -> true
  | _ -> false

let is_green = function
  | Green _ -> true
  | _ -> false

let is_light_green = function
  | LightGreen _ -> true
  | _ -> false

let int_of_id_t = function
  | "min_caml_hp" -> failwith ("int_of_id_t min_caml_hp is not supported.")
  | id ->
    match List.last (String.split id ~on:'.') with
    | Some v -> int_of_string v
    | None -> print_endline id; int_of_string id

let string_of_id_or_imm = function
    V (id_t) -> id_t
  | C (n) -> string_of_int n

let rec find_fundef prog name =
  let Asm.Prog (_, fundefs, _) = prog in
  match List.find fundefs ~f:(fun fundef -> fundef.name = name) with
  | Some (body) -> body
  | None -> failwith "find_fundef is failed"

