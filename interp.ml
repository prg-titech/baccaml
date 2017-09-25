type instruction =
  | Mov
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
;;

let rec interp (program : instruction array) (reg : int array) (pc : int) (rp : int) : 'a =
  let operation = Array.get program pc in
  match operation with
  | Mov -> (* 第一オペランドを第二オペランドにコピー *)
    let _ = reg.(rp + 2) <- Array.get reg (rp + 1) in
    interp program reg (pc + 1) (rp + 3)
  | Add -> (* 第一オペランドを第二オペランドに加算する *)
    let _ = reg.(pc + 2) <- Array.get reg (rp + 1) + Array.get reg (rp + 2) in
    interp program reg (pc + 1) (rp + 3)
;;
