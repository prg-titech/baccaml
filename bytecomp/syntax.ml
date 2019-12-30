(* abstract syntax tree for a simple functional language *)
type var = string
[@@deriving show]

type annot =
  | MethodComp
  | TracingComp
[@@deriving show]

type exp =
  | Unit
  | Int of int
  | Not of exp
  | Var of var
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Eq of exp * exp
  | LT of exp * exp             (* less than *)
  | If of exp * exp * exp
  | Call of annot option * var * exp list
  | Let of var * exp * exp
  | LetRec of fundef * exp
  | Array of exp * exp
  | Get of exp * exp
  | Put of exp * exp * exp
  | For of range * exp * exp    (* loop (condition, body, next) *)
  | TCall of annot option * var * exp list     (* tail call --- internal only *)
[@@deriving show]

and range = Range of var * exp * exp (* var = exp to exp => Range (var, exp, exp) *)

and fundef = {name : var; args : var list; body : exp; annot : annot option}
[@@deriving show]

let rec find_fundefs ?(name = None) = function
  | Unit | Int _ | Var _ | Not _ -> []
  | Add (e1, e2) | Sub (e1, e2) | Mul (e1, e2) | Eq (e1, e2) | LT (e1, e2) ->
     (find_fundefs e1) @ (find_fundefs e2)
  | If (e1, e2, e3) -> (find_fundefs e1) @ (find_fundefs e2) @ (find_fundefs e3)
  | Let (_, e1, e2) -> find_fundefs e1 @ find_fundefs e2
  | Call (_, _, exps) | TCall(_, _, exps) -> exps |> List.map find_fundefs |> List.flatten
  | LetRec (fundef, e) ->
    begin
      match name with
      | Some v -> if fundef.name = v then [fundef] else find_fundefs e
      | None -> fundef :: (find_fundefs e)
    end
  | Array (e1, e2) -> (find_fundefs e1) @ (find_fundefs e2)
  | Get (e1, e2) -> (find_fundefs e1) @ (find_fundefs e2)
  | Put (e1, e2, e3) ->
    (find_fundefs e1) @ (find_fundefs e2) @ (find_fundefs e3)
  | For (_, e2, e3) ->
    (find_fundefs e2) @ (find_fundefs e3)
