let rec interp stack sp bytecode pc =
  if pc = 0 then trace_entry stack sp bytecode else
  if pc = 20 then test_trace_2 stack sp else
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
    let addr = bytecode.(pc + 1) in
    if is_mj () then
      let r = interp stack sp bytecode addr in
      stack.(sp - 1) <- r;
      interp stack sp bytecode (pc + 2)
    else
      (stack.(sp) <- pc + 2;
       interp stack (sp + 1) bytecode addr)
  else if instr = 7 then        (* RET *)
    if is_mj () then
     stack.(sp - 1)
    else
     (let n = bytecode.(pc + 1) in
      let v = stack.(sp - 1) in   (* sp: sp - 1 *)
      let pc2 = stack.(sp - 2) in (* sp: sp - 2 *)
      stack.(sp - n - 2) <- v;    (* sp: sp - 2 - n + 1 = sp - 1 - n *)
      let m = sp - n - 1 in
      can_enter_jit stack m bytecode pc2;
      interp stack m bytecode pc2)
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
    (print_int instr; print_newline ();
    -1000) in
let code = Array.make 100 0 in
let stack = Array.make 10000 0 in
code.(0) <- 8;
code.(1) <- 1;
code.(2) <- 4;
code.(3) <- 2;
code.(4) <- 3;
code.(5) <- 5;
code.(6) <- 11;
code.(7) <- 4;
code.(8) <- 1;
code.(9) <- 14;
code.(10) <- 21;
code.(11) <- 8;
code.(12) <- 1;
code.(13) <- 8;
code.(14) <- 2;
code.(15) <- 4;
code.(16) <- 1;
code.(17) <- 1;
code.(18) <- 6;
code.(19) <- 0;
code.(20) <- 0;
code.(21) <- 7;
code.(22) <- 1;
code.(23) <- 4;
code.(24) <- 1000;
code.(25) <- 6;
code.(26) <- 0;
code.(27) <- 9;
let rec loop x =
  if x = 0 then ()
  else
    let res = interp stack 0 code 23 in
    if res = -1000 then
      print_int (100000000000)
    else
      (loop (x - 1))
in
let start = get_micro_time () in
let _ = loop 100000 in
let stop = get_micro_time () in
print_int (stop - start); print_newline ()
