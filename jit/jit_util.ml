open Mincaml
open Util
open Core

let int_of_id_t id =
  if String.equal id "min_caml_hp" then
    failwith ("int_of_id_t min_caml_hp is not supported.")
  else
    match int_of_string_opt (StringUtil.after_of id '.') with
    | Some (i) -> i
    | _ ->
      match int_of_string_opt (StringUtil.after_of id 'u') with
      | Some (i) -> i
      | _ -> failwith (Printf.sprintf "int_of_id_t (%s) is failed" id)      
