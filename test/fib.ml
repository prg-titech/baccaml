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
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 + v2;
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 1 then        (* SUB *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 - v2;
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 2 then        (* LT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp) <- (if v1 < v2 then 1 else 0);
    interp bytecode (pc + 1) stack (sp + 1)
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
    then interp bytecode addr stack (sp - 2)
    else interp bytecode (pc + 2) stack (sp - 2)
  else if instr = 6 then        (* CALL *)
    let addr = bytecode.(pc + 1) in
    let v = interp bytecode addr stack sp in
    stack.(sp) <- v;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 7 then        (* RET *)
    stack.(sp - 1)
  else if instr = 8 then        (* DUP *)
    let v = stack.(sp - 1) in
    stack.(sp) <- v;
    interp bytecode (pc + 2) stack (sp + 1)
  else
    -1000 in

(*
   CONST 1
   LT
   JUMP_IF_ZERO 8
   CONST 1
   RET
   CONST 1
   SUB
   CALL 0
   CONST 2
   SUB
   CALL 0
   ADD
   RET
 *)

let bytecode = [|
  4; 1;
  2;
  5; 8;
  4; 1;
  7;
  4; 1;
  1;
  6; 0;
  4; 2;
  1;
  6; 0;
  0;
  7;
|] in

let stack = Array.make 10 0 in
stack.(0) <- 10;
print_int (interp bytecode 0 stack 1)
