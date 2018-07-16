(* let loop_end a = () in
   let loop_start b = () in *)

let rec interp bytecode pc a =
  jit_dispatch (pc=3) bytecode a;
  let instr = bytecode.(pc) in
  if instr = 0 then             (* ADD *)
    let b = bytecode.(pc + 1) in
    interp bytecode (pc + 2) (a + b)
  else if instr = 1 then        (* SUB *)
    let b = bytecode.(pc + 1) in
    interp bytecode (pc + 2) (a - b)
  else if instr = 2 then        (* JUMP_IF *)
    let t = bytecode.(pc + 1) in
    if a < 0 then
      if t < pc then
        (loop_end a; interp bytecode t a)
      else
        interp bytecode t a
    else
      interp bytecode (pc + 2) a
  else if instr = 3 then        (* JUMP *)
    let t = bytecode.(pc + 1) in
    if t < pc then
      (loop_end a; interp bytecode t a)
    else
      interp bytecode t a
  else if instr = 4 then        (* LOOP_S *)
    (loop_start a;
     interp bytecode (pc + 1) a)
  else if instr = 5 then        (* LOOP_E *)
    (loop_end a;
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
code.(6) <- 2; code.(7) <- 12;
code.(8) <- 6; code.(9) <- 0;
code.(10) <- 3; code.(11) <- 5;
code.(12) <- 7;
print_int (interp code 3 2147483647)
