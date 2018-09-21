open MinCaml
open Asm

let find_fundef name fundefs =
  List.find begin fun fundef ->
    let Id.L (n) = fundef.name in
    List.for_all (fun x -> String.contains n x) (Stringext.to_list name)
  end fundefs

let transform (Prog (_, fundefs, _)) =
  let interp = find_fundef "interp" fundefs in
  ()
