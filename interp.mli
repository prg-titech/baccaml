type instruction =
    Mov
  | Add
  | Cmp
  | Div
  | Sub
  | And
  | Neg
  | Not
  | Or
  | Jmp
  | Jne
  | Call
  | Ret
  | Pop
  | Push
  | Nop
val interp : instruction array -> int array -> int -> int -> 'a
