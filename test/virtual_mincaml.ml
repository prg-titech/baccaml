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
  else if instr = 3 then        (* LT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- (if v1 < v2 then 1 else 0);
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
  else
    -1000 in

let code = Array.make 50 0 in
let stack = Array.make 50 0 in
code.(0) <- 4;
code.(1) <- 10;
code.(2) <- 6;
code.(3) <- 5;
code.(4) <- 9;
code.(5) <- 8;
code.(6) <- 1;
code.(7) <- 4;
code.(8) <- 2;
code.(9) <- 3;
code.(10) <- 5;
code.(11) <- 18;
code.(12) <- 8;
code.(13) <- 1;
code.(14) <- 4;
code.(15) <- 0;
code.(16) <- 5;
code.(17) <- 33;
code.(18) <- 8;
code.(19) <- 1;
code.(20) <- 4;
code.(21) <- 1;
code.(22) <- 1;
code.(23) <- 6;
code.(24) <- 5;
code.(25) <- 8;
code.(26) <- 2;
code.(27) <- 4;
code.(28) <- 2;
code.(29) <- 1;
code.(30) <- 6;
code.(31) <- 5;
code.(32) <- 0;
code.(33) <- 7;
code.(34) <- 1;
print_int (interp code 0 stack 0)
