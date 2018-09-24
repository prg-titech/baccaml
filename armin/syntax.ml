type var = string
[@@deriving show]

type exp =
  | Int of int
  | Var of var
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | LT of exp * exp
  | If of exp * exp * exp
  | Call of var * exp list
  | TCall of var * exp list
  | Let of var * exp * exp
  | LetRec of fundef * exp
[@@deriving show]

and fundef = {
    name : var;
    args : var list;
    body : exp
  } [@@deriving show]
