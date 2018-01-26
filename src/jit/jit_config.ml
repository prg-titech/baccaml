open Asm
open Core

type value =
  | Red of int
  | Green of int

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

let value_of = function
  | Red (n) -> n
  | Green (n) -> n

let is_red = function
  | Red _ -> true
  | Green _ -> false

let is_green = function
  | Red _ -> false
  | Green _ -> true

let int_of_id_t = function
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
