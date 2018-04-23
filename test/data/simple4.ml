let rec interp bytecode pc a =
  if pc = 6 then test_trace a bytecode else
  let instr = bytecode.(pc) in
  if instr = 0 then (* ADD *)
    interp bytecode (pc + 1) (a + 1)
  else if instr = 1 then (* SUB *)
    interp bytecode (pc + 1) (a - 1)
  else if instr = 2 then (* CALL *)
    let t1 = bytecode.(pc + 1) in
    let v = interp bytecode t1 a in
    let t2 = bytecode.(pc + 2) in
    interp bytecode t2 v
  else if instr = 3 then (* JUMP_IF *)
    let t1 = bytecode.(pc + 1) in
    let t2 = bytecode.(pc + 2) in
    if a > 0 then
      interp bytecode t1 a
    else
      interp bytecode t2 a
  else if instr = 4 then (* RETURN *)
    a
  else if instr = 5 then (* CONST *)
    let a2 = bytecode.(pc + 1) in
    interp bytecode (pc + 2) a2
  else if instr = 6 then (* PRINT_A *)
    (print_int (a);
     a)
  else (* OTHERS *)
    -100
in
let code = Array.make 100 (-1) in
code.(0) <- 1;
code.(1) <- 1;
code.(2) <- 0;
code.(3) <- 4;
code.(4) <- 5; code.(5) <- 10;
code.(6) <- 3; code.(7) <- 9; code.(8) <- 12;
code.(9) <- 2; code.(10) <- 0; code.(11) <- 12;
code.(12) <- 4;
print_int (interp code 4 0)
