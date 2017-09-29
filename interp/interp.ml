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
  | And of int * int
  | Or of int * int
  | Jump of int
  | Jne of int
  | Nop
  | Load of int * int
  | Store of int * int
  | Halt
;;

let rec interp (program : instruction array) (reg : int array) (mem : int array) (pc : int) : 'a =
  let operation = Array.get program pc in
  match operation with
  | Mov (x, y) ->
    let op1 = reg.(x) in
    reg.(y) <- op1;
    interp program reg mem (pc + 1)
  | MovImm (x, y) ->
    reg.(y) <- x;
    interp program reg mem (pc + 1)
  | Add (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    reg.(y) <- (r1 + r2);
    interp program reg mem (pc + 1)
  | AddImm (x, y) ->
    let r2 = reg.(y) in
    reg.(y) <- (x + r2);
    interp program reg mem (pc + 1)
  | Cmp (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    if r1 < r2 then reg.(y) <- 1
    else reg.(y) <- 0;
    interp program reg mem (pc + 1)
  | CmpImm (x, y) ->
    let r2 = reg.(y) in
    if x < r2 then reg.(y) <- 1
    else reg.(y) <- 0;
    interp program mem reg (pc + 1)
  | Div x ->
    let r1 = reg.(x) in
    reg.(0) <- (reg.(0) / r1);
    reg.(1) <- (reg.(0) mod r1);
    interp program reg mem (pc + 1)
  | DivImm x ->
    reg.(0) <- (reg.(0) / x);
    reg.(1) <- (reg.(0) mod x);
    interp program reg mem (pc + 1)
  | Sub (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    reg.(y) <- (r2 - r1);
    interp program reg mem (pc + 1)
  | SubImm (x, y) ->
    let r2 = reg.(y) in
    reg.(y) <- (r2 - y);
    interp program reg mem (pc + 1)
  | And (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    if r1 = 1 && r2 = 1 then reg.(y) <- 1
    else reg.(y) <- 0;
    interp program reg mem (pc + 1)
  | Or (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    if r1 = 1 || r2 = 1 then reg.(y) <- 1
    else reg.(y) <- 0;
    interp program reg mem (pc + 1)
  | Jump address ->
    interp program reg mem address
  | Jne address ->
    let flag = reg.(0) in
    if flag = 0 then
      interp program reg mem address
    else
      interp program reg mem (pc + 1)
  | Nop ->
    interp program reg mem (pc + 1)
  | Load (x, y) ->
    let r1 = reg.(x) in
    let m1 = mem.(r1) in
    reg.(y) <- m1;
    interp program reg mem (pc + 1)
  | Store (x, y) ->
    let r1 = reg.(x) in
    let r2 = reg.(y) in
    mem.(r2) <- r1;
    interp program reg mem (pc + 1)
  | Halt ->
    reg
;;

let add1 = [| Add (0, 1); Halt |]
;;

(*
  # r0 <- n
  int r1 = 0;
  int r2 = 0;

 l1:
    r1 += r2;
    r2 += 1;
    if (r2 != r0) goto l1;
*)
let sum1 = [| MovImm(0, 1); MovImm(0, 2); Add(2, 1); AddImm(1, 2); Cmp(0, 2); Jne(2); Halt |]
;;

let () =
  let reg = Array.make 256 0 in
  let mem = [||] in
  let pc = 0 in
  reg.(0) <- 10;
  let res = interp sum1 reg mem pc in
  print_int res.(1)
