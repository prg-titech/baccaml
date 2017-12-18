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
  else if instr = 2 then (* SubImm *)
    let v = stack.(sp) in
    let n = bytecode.(pc + 1) in
    stack.(sp - 1) <- (v - n);
    interp bytecode (pc + 2) stack (sp - 1)
  else if instr = 10 then (* Call *)
    let addr = bytecode.(pc + 1) in
    interp bytecode addr stack sp
  else if instr = 11 then (* Ret *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp) in
    let raddr = stack.(sp - 1) in
    stack.(sp - n - 1) <- v;
    interp bytecode raddr stack (sp - n - 1)
  else if instr = 20 then (* POP1 *)
    let v = stack.(sp) in
    let _ = stack.(sp - 1)in
    stack.(sp - 1) <- v;
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 21 then (* PUSH *)
    let n = bytecode.(pc + 1) in
    stack.(sp + 1) <- n;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 22 then (* Dup *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n) in
    stack.(sp + 1) <- v;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 30 then (* Halt *)
    stack.(sp)
  else
    -1
in
let code  = Array.make 10 0 in
let stack = Array.make 10 0 in
code.(0) <- 21; code.(1) <- 2;
code.(2) <- 21; code.(3) <- 3;
code.(4) <- 22; code.(5) <- 1;
code.(6) <- 0;
code.(7) <- 30;
print_int (interp code 0 stack 0)
