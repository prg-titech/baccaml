open MinCaml
open Asm
open Std
open Jit_env

let zero = "zero.0"

let int_of_id_t id =
  let str = Str.regexp "[.\\s]+" in
  Str.split str id |> List.last |> int_of_string
;;

let int_of_id_or_imm = function V id -> int_of_id_t id | C n -> n
let value_of = function Red n -> n | Green n -> n
let is_red = function Red _ -> true | _ -> false
let is_green = function Green _ -> true | _ -> false
