open Core

include String

let get_prefix str =
  String.split str ~on:'.'
  |> List.hd
  |> Option.value ~default:str

let contains str1 str2 =
  let re = Str.regexp_string str2 in
  try ignore (Str.search_forward re str1 0); true
  with _ -> false
