let print_array f arr =
  print_string "[|";
  Array.iter
    (fun a -> f a; print_string "; ")
    arr;
  print_string "|] " in

let loop_end a b = () in

(* ADD 0
   SUB 1
   MUL 2
   LT  3
   CONST 4
   JUMP_If_ZERO 5
   CALL 6
   RET 7
   DUP 8
   HALT 9
   FRAME_RESET 10
   POP1 11
   LOOP_S 12
*)
let rec interp bytecode stack pc sp =
  let instr = bytecode.(pc) in
  Printf.printf "is: %d\tsp: %d\tpc: %d\t" instr sp pc;
  print_array print_int stack; print_newline ();
  if instr = 0 then             (* ADD *)
    let v2 = stack.(sp - 1) in  (* sp: sp - 1 *)
    let v1 = stack.(sp - 2) in  (* sp: sp - 2 *)
    stack.(sp - 2) <- v1 + v2;  (* sp: sp - 1 *)
    interp bytecode stack (pc + 1) (sp - 1)
  else if instr = 1 then        (* SUB *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 - v2;
    interp bytecode stack (pc + 1) (sp - 1)
  else if instr = 3 then        (* LT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- (if v1 < v2 then 1 else 0);
    interp bytecode stack (pc + 1) (sp - 1)
  else if instr = 4 then        (* CONST *)
    let c = bytecode.(pc + 1) in
    stack.(sp) <- c;
    interp bytecode stack (pc + 2) (sp + 1)
  else if instr = 5 then        (* JUMP_IF_ZERO *)
    let addr = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in
    if v = 0 then
      (if addr < pc then loop_end bytecode stack;
       interp bytecode stack addr (sp - 1))
    else
      interp bytecode stack (pc + 2) (sp - 1)
  else if instr = 6 then        (* CALL *)
    let addr = bytecode.(pc + 1) in
    stack.(sp) <- (pc + 2);
    interp bytecode stack addr (sp + 1)
  else if instr = 7 then        (* RET *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in   (* sp: sp - 1 *)
    let pc2 = stack.(sp - 2) in (* sp: sp - 2 *)
    stack.(sp - n - 2) <- v;    (* sp: sp - 2 - n + 1 = sp - 1 - n *)
    interp bytecode stack pc2 (sp - n - 1)
  else if instr = 8 then        (* DUP *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n - 1) in
    stack.(sp) <- v;
    interp bytecode stack (pc + 2) (sp + 1)
  else if instr = 9 then        (* HALT *)
    stack.(sp - 1)
  else if instr = 10 then       (* MUL *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 * v2;
    interp bytecode stack (pc + 1) (sp - 1)
  else if instr = 11 then       (* JUMP *)
    let addr = bytecode.(pc + 1) in
    interp bytecode stack addr sp
  else if instr = 12 then       (* LOOP_S *)
    interp bytecode stack (pc + 1) sp
  else if instr = 13 then      (* LOOP_E *)
    (loop_end bytecode stack;
     interp bytecode stack (pc + 1) sp)
  else
    -1000 in

let (===) res expected =
  if res = expected then
    Printf.printf "PASS "
  else
    Printf.printf "FAIL ";
  Printf.printf "expected: %d, result: %d\n" expected res in

(* simple test *)
let code_simple = [|
  4; 1;
  4; 2;
  0;
  9
|] in

let stack_simple = Array.make 10 0 in
(interp code_simple stack_simple 0 0) === 3;

(* call test *)
(* CONST 10
   CONST 20
   CALL 7
   HALT
   DUP 2
   DUP 2
   ADD
   RET 1
*)
let code_call = [|
  4; 10;
  4; 20;
  6; 7;
  9;
  8; 2;
  8; 2;
  0;
  7; 1
|] in
let stack_call = Array.make 10 0 in
(interp code_call stack_call 0 0) === 30;

(* jump if and call test *)
(* CONST 1
   CONST 2
   ADD
   DUP 0
   JUMP_IF_ZERO 13
   CALL 14
   JUMP 5
   HALT
   DUP 1
   CONST 1
   DUB
   RET 1
*)
let code_jmp_if = [|
  4; 1;
  4; 2;
  0;
  8; 0;
  5; 13;
  6; 14;
  11; 5;
  9;
  8; 1;
  4; 1;
  1;
  7; 1;
|] in

let stack_jmp_if = Array.make 10 0 in
(interp code_jmp_if stack_jmp_if 0 0) === 0;

let code_fib =
  [|4; 10; 6; 6; 9; 12; 8; 1; 4; 2; 3; 5; 19; 8; 1; 4; 0; 5; 34; 8; 1; 4; 1; 1; 6; 6; 8; 2; 4; 2; 1; 6; 6; 0; 7; 1 |]
  (* [| 4; 10; 6; 5; 9; 8; 1; 4; 2; 3; 5; 18; 8; 1; 4; 0; 5; 33; 8; 1; 4; 1; 1; 6; 5; 8; 2; 4; 2; 1; 6; 5; 0; 7; 1 |] *)
in
let stack_fib = Array.make 30 0 in
(interp code_fib stack_fib 0 0) === 55;
