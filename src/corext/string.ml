open Core

include String

let get_prefix str =
  String.split str ~on:'.'
  |> List.hd
  |> Option.value ~default:str
