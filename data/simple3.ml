let rec interp bytecode pc stack sp =
  let instr = bytecode.(pc) in
  if instr = 0 then (* Add *)
    let v2 = stack.(sp) in
    let v1 = stack.(sp - 1) in
    stack.(sp - 1) <- (v1 + v2);
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 1 then (* Sub *)
    let v2 = stack.(sp) in
    let v1 = stack.(sp - 1) in
    stack.(sp - 1) <- (v1 - v2);
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 4 then (* CONST *)
    let n = bytecode.(pc + 1) in
    stack.(sp) <- n;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 10 then (* CALL *)
    let addr = bytecode.(pc + 1) in
    stack.(sp + 1) <- (pc + 2);
    interp bytecode addr stack (sp + 1)
  else if instr = 11 then (* RET *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp) in
    let raddr = stack.(sp - 1) in
    stack.(sp - n - 1) <- v;
    interp bytecode raddr stack (sp - n - 1)
  else if instr = 22 then (* DUP *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n) in
    stack.(sp + 1) <- v;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 30 then (* HALT *)
    stack.(sp)
  else
    -1
in
let code = Array.make 100 0 in
let stack = Array.make 100 0 in
stack.(0) <- 4; stack.(1) <- 5;
code.(0) <- 22; code.(1) <- 2;
code.(2) <- 22; code.(3) <- 2;
code.(4) <- 0;
code.(5) <- 11; code.(6) <- 2;
code.(7) <- 10; code.(8) <- 0;
code.(9) <- 30;
print_int (interp code 7 stack 1)
