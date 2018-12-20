type exp = Inst of int

let print_exp = function
  | Inst i -> print_int i
