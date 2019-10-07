open Base
open Asm
open Std
open Jit_env

let zero = "zero.0"

let int_of_id_t id =
  try
    id
    |> String.split_on_char '.'
    |> List.last
    |> int_of_string
  with e -> id |> String.split_on_char 'u' |> List.last |> int_of_string

let int_of_id_or_imm = function
  | V (id) -> int_of_id_t id
  | C (n) -> n

let value_of = function
    Red n | Green n | LightGreen n -> n

let is_red = function
    Red _ -> true
  | _ -> false

let is_green = function
    Green _ -> true
  | _ -> false

let is_light_green = function
    LightGreen _ -> true
  | _ -> false
