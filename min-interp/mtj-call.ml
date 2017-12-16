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
  else if instr = 3 then (* Push*)
    let n = bytecode.(pc + 1) in
    stack.(sp + 1) <- n;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 4 then (* Dup *)
    let v = stack.(sp) in
    stack.(sp + 1) <- v;
    interp bytecode (pc + 1) stack (sp + 1)
  else if instr = 10 then (* Call *)
    let addr = bytecode.(pc + 1) in
    interp bytecode addr stack sp
  else if instr = 11 then (* Jump_if_zero *)
    let addr = bytecode.(pc + 1) in
    let v = stack.(sp) in
    if v = 0 then
      interp bytecode (pc + 2) stack sp
    else
      interp bytecode addr stack sp
  else if instr = 20 then (* Halt *)
    stack.(sp)
  else
    -1
in
let code = Array.make 100 0 in
let stack = Array.make 10 0 in
code.(0) <- 3; code.(1) <- 10;
code.(2) <- 3; code.(3) <- 20;
code.(4) <- 0;
code.(5) <- 4;
code.(6) <- 0;
code.(7) <- 2; code.(8) <- 1;
code.(9) <- 11; code.(10) <- 7;
code.(11) <- 20;
print_int (interp code 0 stack 0)
