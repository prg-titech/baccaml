let rec interp bytecode pc stack sp =
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
    stack.(sp - n - 2) <- v;    (* sp: sp - 2 - n + 1 = sp - 1 - n *)
    interp bytecode pc2 stack (sp - n - 1)
  else if instr = 8 then        (* DUP *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n - 1) in
    stack.(sp) <- v;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 9 then        (* HALT *)
    stack.(sp - 1)
  else if instr = 11 then       (* JUMP *)
    let addr = bytecode.(pc + 1) in
    interp bytecode addr stack sp
  else
    -1000 in
let code = Array.make 10 0 in
let stack = Array.make 10 0 in
print_int (interp code 0 stack 0)
