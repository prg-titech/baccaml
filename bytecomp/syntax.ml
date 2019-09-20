(* abstract syntax tree for a simple functional language *)
type var = string
[@@deriving show]
type exp =
  | Int of int
  | Var of var
  | Add of exp * exp
  | Mul of exp * exp
  | LT of exp * exp             (* less than *)
  | If of exp * exp * exp
  | Call of var * exp list
  | Let of var * exp * exp
  | LetRec of fundef * exp
  | Main of exp
  | TCall of var * exp list     (* tail call --- internal only *)
[@@deriving show]
and fundef = {name:var; args:var list; body: exp}
[@@deriving show]
