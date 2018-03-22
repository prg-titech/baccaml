open Asm
open Core
open Util

let int_of_id_t id =
  if String.equal id "min_caml_hp" then
    failwith ("int_of_id_t min_caml_hp is not supported.")
  else
    try
      int_of_string (StringUtil.after_of id '.')
    with _ ->
      int_of_string (StringUtil.after_of id 'u')
