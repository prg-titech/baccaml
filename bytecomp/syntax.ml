(* abstract syntax tree for a simple functional language *)
type var = string
[@@deriving show]
type exp =
  | Unit
  | Int of int
  | Var of var
  | Add of exp * exp
  | Mul of exp * exp
  | LT of exp * exp             (* less than *)
  | If of exp * exp * exp
  | Call of var * exp list
  | Let of var * exp * exp
  | LetRec of fundef * exp
  | TCall of var * exp list     (* tail call --- internal only *)
[@@deriving show]
and fundef = {name:var; args:var list; body: exp}
[@@deriving show]

let rec find_fundefs ?(name = None) exp = match exp with
  | Unit | Int _ | Var _ -> []
  | Add (e1, e2) | Mul (e1, e2) | LT (e1, e2) -> (find_fundefs e1) @ (find_fundefs e2)
  | If (e1, e2, e3) -> (find_fundefs e1) @ (find_fundefs e2) @ (find_fundefs e3)
  | Let (_, e1, e2) -> find_fundefs e1 @ find_fundefs e2
  | Call (_, exps) -> exps |> List.map find_fundefs |> List.flatten
  | LetRec (fundef, e) ->
    begin match name with
      | Some v ->
        let { name = fname'; } = fundef in
        if fname' = v then [fundef] else find_fundefs e
      | None ->
        fundef :: (find_fundefs e)
    end
  | _ -> []
