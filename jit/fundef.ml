open Base

open Std

type t = Asm.fundef

let find (Asm.Prog (_, fundefs, _)) elem =
  fundefs |> List.find (fun { Asm.name } -> name = elem)

let find_opt (Asm.Prog (_, fundefs, _)) elem =
  fundefs |> List.find_opt (fun { Asm.name } -> name = elem)

let find_fuzzy (Asm.Prog (_, fundefs, _)) ~name =
  fundefs |> List.find (fun {Asm.name= Id.L x} -> String.contains x name)

let fund_fuzzy_opt (Asm.Prog (_, fundefs, _)) ~name =
  fundefs |> List.find_opt (fun {Asm.name= Id.L x} -> String.contains x name)
