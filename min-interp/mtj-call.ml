let rec interp bytecode pc stack sp =
  let instr = bytecode.(pc) in
  if instr = 0 then
    let v2 = stack.(sp) in
    let v1 = stack.(sp - 1) in
    stack.(sp - 1) <- (v1 + v2);
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 2 then
    let n = bytecode.(pc + 1) in
    stack.(sp + 1) <- n;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 3 then
    stack.(sp)
  else
    -1
in
let code = Array.make 10 0 in
let stack = Array.make 10 0 in
code.(0) <- 2; code.(1) <- 10;
code.(2) <- 2; code.(3) <- 20;
code.(4) <- 0;
code.(5) <- 3;
print_int (interp code 0 stack 0)
