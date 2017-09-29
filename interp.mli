type instruction =
    Mov of int * int
  | MovImm of int * int
  | Add of int * int
  | AddImm of int * int
  | Cmp of int * int
  | CmpImm of int * int
  | Div of int
  | DivImm of int
  | Sub of int * int
  | SubImm of int * int
  | And of int * int
  | Or of int * int
  | Jump of int
  | Jne of int
  | Nop
  | Load of int * int
  | Store of int * int
  | Halt
val interp : instruction array -> int array -> int array -> int -> int array
val add1 : instruction array
val sum1 : instruction array
