let rec interp stack sp bytecode pc =
  (* jit_dispatch (pc=0) stack sp bytecode; *)
  let instr = bytecode.(pc) in
  if instr = 0 then             (* ADD *)
    let v2 = stack.(sp - 1) in  (* sp: sp - 1 *)
    let v1 = stack.(sp - 2) in  (* sp: sp - 2 *)
    stack.(sp - 2) <- v1 + v2;  (* sp: sp - 1 *)
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 1 then        (* SUB *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 - v2;
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 3 then        (* LT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    let n = (if v1 < v2 then 1 else 0) in
    stack.(sp - 2) <- n;
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 4 then        (* CONST *)
    let c = bytecode.(pc + 1) in
    stack.(sp) <- c;
    interp stack (sp + 1) bytecode (pc + 2)
  else if instr = 5 then        (* JUMP_IF_ZERO *)
    let addr = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in
    if v = 0
    then interp stack (sp - 1) bytecode addr
    else interp stack (sp - 1) bytecode (pc + 2)
  else if instr = 6 then        (* CALL *)
    (mj_call_start ();
     let addr = bytecode.(pc + 1) in
     let r = interp stack sp bytecode (bytecode.(pc + 1)) in
     stack.(sp - 1) <- r;
     let res = interp stack sp bytecode (pc + 2) in
     mj_call_end (res);
     stack.(sp) <- pc + 2;
     interp stack (sp + 1) bytecode (bytecode.(pc + 1)))
  else if instr = 7 then        (* RET *)
    (mj_ret_start ();
     let res = stack.(sp - 1) in
     mj_ret_end (res);
     let n = bytecode.(pc + 1) in
     let v = stack.(sp - 1) in   (* sp: sp - 1 *)
     let pc2 = stack.(sp - 2) in (* sp: sp - 2 *)
     stack.(sp - n - 2) <- v;    (* sp: sp - 2 - n + 1 = sp - 1 - n *)
     interp stack (sp - n - 1) bytecode pc2)
  else if instr = 8 then        (* DUP *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n - 1) in
    stack.(sp) <- v;
    interp stack (sp + 1) bytecode (pc + 2)
  else if instr = 9 then        (* HALT *)
    stack.(sp - 1)
  else if instr = 11 then       (* POP1 *)
    let v = stack.(sp - 1) in
    let _ = stack.(sp - 2) in
    stack.(sp - 2) <- v;
    interp stack (sp - 2) bytecode (pc + 1)
  else if instr = 14 then       (* JUMP *)
    let addr = bytecode.(pc + 1) in
    interp stack sp bytecode addr
  else
    (print_int instr;
     print_newline ();
     print_int pc;
     print_newline ();
    -1000) in
let code = Array.make 100 0 in
let stack = Array.make 10000 0 in
code.(0) <- 8;
code.(1) <- 2;
code.(2) <- 4;
code.(3) <- 1;
code.(4) <- 3;
code.(5) <- 5;
code.(6) <- 11;
code.(7) <- 8;
code.(8) <- 1;
code.(9) <- 14;
code.(10) <- 56;
code.(11) <- 8;
code.(12) <- 2;
code.(13) <- 8;
code.(14) <- 2;
code.(15) <- 3;
code.(16) <- 5;
code.(17) <- 38;
code.(18) <- 8;
code.(19) <- 1;
code.(20) <- 8;
code.(21) <- 3;
code.(22) <- 1;
code.(23) <- 8;
code.(24) <- 3;
code.(25) <- 8;
code.(26) <- 1;
code.(27) <- 10;
code.(28) <- 2;
code.(29) <- 1;
code.(30) <- 2;
code.(31) <- 4;
code.(32) <- 0;
code.(33) <- 5;
code.(34) <- 0;
code.(35) <- 11;
code.(36) <- 14;
code.(37) <- 56;
code.(38) <- 8;
code.(39) <- 2;
code.(40) <- 8;
code.(41) <- 2;
code.(42) <- 1;
code.(43) <- 8;
code.(44) <- 2;
code.(45) <- 8;
code.(46) <- 1;
code.(47) <- 10;
code.(48) <- 2;
code.(49) <- 1;
code.(50) <- 2;
code.(51) <- 4;
code.(52) <- 0;
code.(53) <- 5;
code.(54) <- 0;
code.(55) <- 11;
code.(56) <- 7;
code.(57) <- 2;
code.(58) <- 4;
code.(59) <- 10;
code.(60) <- 4;
code.(61) <- 18;
code.(62) <- 6;
code.(63) <- 0;
code.(64) <- 9;
let res = interp stack 0 code 58 in
print_int res; print_newline ()
