open Core
open Mincaml
open Util.Xutil
open Asm
open Jit_config

let int_of_id_t id =
  if String.equal id "min_caml_hp" then
    failwith ("int_of_id_t min_caml_hp is not supported.")
  else
    match int_of_string_opt (String.after_of id '.') with
    | Some (i) -> i
    | _ ->
      match int_of_string_opt (String.after_of id 'u') with
      | Some (i) -> i
      | _ -> failwith (Printf.sprintf "int_of_id_t (%s) is failed" id)      


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
