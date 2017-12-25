open Asm

exception Not_supported of string

exception Un_implemented_instruction of string

type labels = (* function label for closures *)
  (Id.l * int) list

type prog_with_label = (* prog for interpreter *)
  ProgWithLabel of (Id.l * float) list * fundef list * t * labels

let register_size = 1000000

let heap = ref 0

let to_prog_with_label prog =
  let rec create_labels fundefs i =
    match fundefs with
    | [] -> []
    | fundef :: tl -> (fundef.name, i) :: create_labels tl (i + 1)
  in
  let Prog (table, fundefs, exp) = prog in
  let labels = create_labels fundefs 0 in
  ProgWithLabel (table, fundefs, exp, labels)
