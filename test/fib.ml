let print_array f arr =
  print_string "[|";
  Array.iter
    (fun a -> f a; print_string "; ")
    arr;
  print_string "|] " in

let rec interp bytecode pc stack sp =
  print_string @@ "pc: " ^ (string_of_int pc) ^ " sp: " ^ (string_of_int sp);
  print_array print_int stack; print_newline ();
  let instr = bytecode.(pc) in
  if instr = 0 then             (* ADD *)
    let v2 = stack.(sp - 1) in  (* sp: sp - 1 *)
    let v1 = stack.(sp - 2) in  (* sp: sp - 2 *)
    stack.(sp - 2) <- v1 + v2;  (* sp: sp - 1 *)
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 1 then        (* SUB *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 - v2;
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 2 then        (* LT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- (if v1 < v2 then 1 else 0);
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 3 then        (* GT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- (if v1 > v2 then 1 else 0);
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 4 then        (* CONST *)
    let c = bytecode.(pc + 1) in
    stack.(sp) <- c;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 5 then        (* JUMP_IF_ZERO *)
    let addr = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in
    if v = 0
    then interp bytecode addr stack (sp - 1)
    else interp bytecode (pc + 2) stack (sp - 1)
  else if instr = 6 then        (* CALL *)
    let addr = bytecode.(pc + 1) in
    stack.(sp) <- (pc + 2);
    interp bytecode addr stack (sp + 1)
  else if instr = 7 then        (* RET *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in   (* sp: sp - 1 *)
    let pc2 = stack.(sp - 2) in (* sp: sp - 2 *)
    let sp2 = sp - 2 - n in     (* sp: sp' - n = sp - 2 - n *)
    stack.(sp2) <- v;           (* sp: sp - 2 - n + 1 = sp - 1 - n *)
    interp bytecode pc2 stack (sp2 + 1)
  else if instr = 8 then        (* DUP *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n - 1) in
    stack.(sp) <- v;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 9 then        (* HALT *)
    stack.(sp - 1)
  else if instr = 10 then       (* MUL *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 * v2;
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 11 then       (* JUMP *)
    let addr = bytecode.(pc + 1) in
    interp bytecode addr stack sp
  else
    -1000 in

let (===) res expected =
  if res = expected then print_string "PASS " else print_string "FAIL ";
  Printf.printf "expected: %d, result: %d\n" expected res in

(* simple test *)
let code_simple = [|
  4; 1;
  4; 2;
  0;
  9
|] in

let stack_simple = Array.make 10 0 in
(interp code_simple 0 stack_simple 0) === 3;

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
(interp code_call 0 stack_call 0) === 30;

(* jump if and call test *)
let code_jmp_if = [|
  4; 1;
  4; 2;
  0;
  8; 0;
  5; 14;
  4; 1;
  1;
  11; 5;
  9;
|] in

let stack_jmp_if = Array.make 10 0 in
(interp code_jmp_if 0 stack_jmp_if 0) === 0
