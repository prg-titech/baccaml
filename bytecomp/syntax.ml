(* abstract syntax tree for a simple functional language *)
type var = string
[@@deriving show]

type exp =
  | Unit
  | Int of int
  | Var of var
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Eq of exp * exp
  | LT of exp * exp             (* less than *)
  | If of exp * exp * exp
  | Call of var * exp list
  | Let of var * exp * exp
  | LetRec of fundef * exp
  | Array of exp * exp
  | Get of exp * exp
  | Put of exp * exp * exp * exp
  | For of range * exp * exp    (* loop (condition, body, next) *)
  | While of exp * exp * exp    (* while cond, body, next *)
  | TCall of var * exp list     (* tail call --- internal only *)
[@@deriving show]

and range = Range of var * exp * exp (* var = exp to exp => Range (var, exp, exp) *)

and fundef = {name:var; args:var list; body: exp}
[@@deriving show]

let rec find_fundefs ?(name = None) exp = match exp with
  | Unit | Int _ | Var _ -> []
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Eq (e1, e2) | LT (e1, e2) ->
     (find_fundefs e1) @ (find_fundefs e2)
  | If (e1, e2, e3) -> (find_fundefs e1) @ (find_fundefs e2) @ (find_fundefs e3)
  | Let (_, e1, e2) -> find_fundefs e1 @ find_fundefs e2
  | Call (_, exps) | TCall(_, exps) -> exps |> List.map find_fundefs |> List.flatten
  | LetRec (fundef, e) ->
    begin match name with
    | Some v -> if fundef.name = v then [fundef] else find_fundefs e
    | None -> fundef :: (find_fundefs e)
    end
  | Array (e1, e2) -> (find_fundefs e1) @ (find_fundefs e2)
  | Get (e1, e2) -> (find_fundefs e1) @ (find_fundefs e2)
  | Put (e1, e2, e3, e4) ->
    (find_fundefs e1) @ (find_fundefs e2) @ (find_fundefs e3) @ (find_fundefs e4)
  | For (_, e2, e3) ->
    (find_fundefs e2) @ (find_fundefs e3)
