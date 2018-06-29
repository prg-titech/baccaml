let rec interp bytecode pc a =
  jit_dispatch bytecode (pc=3);
  let instr = bytecode.(pc) in
  if instr = 0 then             (* ADD *)
    let b = bytecode.(pc + 1) in
    interp bytecode (pc + 2) (a + b)
  else if instr = 1 then        (* SUB *)
    let b = bytecode.(pc + 1) in
    interp bytecode (pc + 2) (a - b)
  else if instr = 2 then        (* JUMP_IF *)
    let t = bytecode.(pc + 1) in
    if a > 0 then
      interp bytecode t a
    else
      interp bytecode (pc + 2) a
  else if instr = 3 then        (* JUMP *)
    let t = bytecode.(pc + 1) in
    interp bytecode t a
  else if instr = 4 then        (* LOOP_S *)
    (loop_start pc;
    interp bytecode (pc + 1) a)
  else if instr = 5 then        (* LOOP_E *)
    (loop_end pc;
     interp bytecode (pc + 1) a)
  else if instr = 6 then        (* CALL *)
    let t = bytecode.(pc + 1) in
    let v = interp bytecode t a in
    interp bytecode (pc + 2) v
  else if instr = 7 then        (* RET *)
    a
  else -1000
in
let code = Array.make 100 (-1) in
code.(0) <- 1; code.(1) <- 1;
code.(2) <- 7;
code.(3) <- 1; code.(4) <- 10;
code.(5) <- 4;
code.(6) <- 2; code.(7) <- 13;
code.(8) <- 6; code.(9) <- 0;
code.(10) <- 5;
code.(11) <- 3; code.(12) <- 5;
code.(13) <- 7;
print_int (interp code 3 100)
