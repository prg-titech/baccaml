let rec interp stack sp bytecode pc =
  jit_dispatch (pc=0) stack sp bytecode;
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
    -1000) in
let code = Array.make 100 0 in
let stack = Array.make 10000 0 in
code.(0) <- 8;
code.(1) <- 0;
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
code.(12) <- 0;
code.(13) <- 8;
code.(14) <- 1;
code.(15) <- 4;
code.(16) <- 1;
code.(17) <- 1;
code.(18) <- 6;
code.(19) <- 0;
code.(20) <- 0;
code.(21) <- 7;
code.(22) <- 4;
code.(23) <- 5000;
code.(24) <- 6;
code.(25) <- 0;
code.(26) <- 9;
(* let start = get_micro_time () in
 * let rec loop n =
 *   if n = 0 then ()
 *   else
 *     let res = interp stack 0 code 22 in
 *     loop (n -1)
 * in
 * loop 1;
 * let stop = get_micro_time () in
 * print_int (stop - start); print_newline () *)
let start = get_micro_time () in
let res = interp stack 0 code 22 in
let stop = get_micro_time () in
print_int (stop - start); print_newline ()
