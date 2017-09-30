open Core

(* x86 命令セット *)
(* オペランドの順番は GAS 形式 *)
type instruction =
  | Mov of int * int
  | MovImm of int * int
  | Add of int * int
  | AddImm of int * int
  | Cmp of int * int
  | CmpImm of int * int
  | Div of int
  | DivImm of int
  | Sub of int * int
  | SubImm of int * int
  | Mul of int * int
  | MulImm of int * int
  | And of int * int
  | Or of int * int
  | Jump of int
  | JumpIf of int
  | Jne of int
  | Nop
  | Load of int * int
  | Store of int * int
  | Halt

type program = instruction array
type register = int array
type memory = int array
type flag = int array
type program_counter = int

let rec interp (program : program) (reg : register) (mem : memory) (flag : flag) (pc : program_counter) : register =
  match program.(pc) with
  | Mov (x, y) ->
    let op1 = reg.(x) in
    reg.(y) <- op1;
    interp program reg mem flag (pc + 1)
  | MovImm (x, y) ->
    reg.(y) <- x;
    interp program reg mem flag (pc + 1)
  | Add (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    reg.(y) <- (r1 + r2);
    interp program reg mem flag (pc + 1)
  | AddImm (x, y) ->
    let r2 = reg.(y) in
    reg.(y) <- (x + r2);
    interp program reg mem flag (pc + 1)
  | Cmp (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    if r1 < r2 then flag.(0) <- 1
    else if r1 = r2 then flag.(1) <- 1
    else flag.(0) <- 0;
    interp program reg mem flag (pc + 1)
  | CmpImm (x, y) ->
    let r2 = reg.(y) in
    if x < r2 then flag.(0) <- 1
    else if x = r2 then flag.(1) <- 1
    else flag.(0) <- 0;
    interp program mem reg flag (pc + 1)
  | Div x ->
    let r1 = reg.(x) in
    let n = reg.(0) in
    reg.(0) <- (n / r1);
    reg.(1) <- (n mod r1);
    interp program reg mem flag (pc + 1)
  | DivImm x ->
    let n = reg.(0) in
    reg.(0) <- (n / x);
    reg.(1) <- (n mod x);
    interp program reg mem flag (pc + 1)
  | Sub (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    reg.(y) <- (r2 - r1);
    interp program reg mem flag (pc + 1)
  | SubImm (x, y) ->
    let r2 = reg.(y) in
    reg.(y) <- (r2 - y);
    interp program reg mem flag (pc + 1)
  | Mul (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    reg.(y) <- (r1 * r2);
    interp program reg mem flag (pc + 1)
  | MulImm (x, y) ->
    let r2 = reg.(y) in
    reg.(y) <- (x * r2);
    interp program reg mem flag (pc + 1)
  | And (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    if r1 = 1 && r2 = 1 then
      reg.(y) <- 1
    else
      reg.(y) <- 0;
    interp program reg mem flag (pc + 1)
  | Or (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    if r1 = 1 || r2 = 1 then
      reg.(y) <- 1
    else
      reg.(y) <- 0;
    interp program reg mem flag (pc + 1)
  | Jump address ->
    interp program reg mem flag address
  | JumpIf address ->
    if (flag.(0)) = 1 then
      interp program reg mem flag address
    else
      interp program reg mem flag address
  | Jne address ->
    if (flag.(1)) = 0 then
      interp program reg mem flag address
    else
      interp program reg mem flag (pc + 1)
  | Nop ->
    interp program reg mem flag (pc + 1)
  | Load (x, y) ->
    let r1 = reg.(x) in
    let m1 = mem.(r1) in
    reg.(y) <- m1;
    interp program reg mem flag (pc + 1)
  | Store (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    mem.(r2) <- r1;
    interp program reg mem flag (pc + 1)
  | Halt ->
    reg
