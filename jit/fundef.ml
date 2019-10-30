open Std
open MinCaml
open Asm

type t = fundef

let create_fundef ~name ~args ~fargs ~body ~ret =
  { name; args; fargs; body; ret }

let find (Asm.Prog (_, _, fundefs, _)) elem =
  fundefs |> List.find (fun { Asm.name } -> name = elem)

let find_opt (Asm.Prog (_, _, fundefs, _)) elem =
  fundefs |> List.find_opt (fun { Asm.name } -> name = elem)

let find_fuzzy (Asm.Prog (_, _, fundefs, _)) ~name =
  fundefs |> List.find (fun {Asm.name= Id.L x} -> String.contains x name)

let fund_fuzzy_opt (Asm.Prog (_, _, fundefs, _)) ~name =
  fundefs |> List.find_opt (fun {Asm.name= Id.L x} -> String.contains x name)
