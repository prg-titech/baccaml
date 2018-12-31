(* let print_array f arr =
 *   print_string "[|";
 *   Array.iter
 *     (fun a -> f a; print_string "; ")
 *     arr;
 *   print_string "|] " in
 *
 * let loop_start _ = () in
 * let loop_end _ = () in
 * let jit_dispatch _ _ _ _ = () in *)

let rec interp stack sp bytecode pc =
  if pc = 0 then trace_entry stack sp else
  if pc = 26 then test_trace_1 stack sp else
  let instr = bytecode.(pc) in
  (* Printf.printf "is: %d\tsp: %d\tpc: %d\t" instr sp pc;
   * print_array print_int stack; print_newline (); *)
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
    if is_mj () then
      let addr = bytecode.(pc + 1) in
      let r = interp stack sp bytecode (bytecode.(pc + 1)) in
      stack.(sp - 1) <- r;
      interp stack sp bytecode (pc + 2)
    else
     (stack.(sp) <- pc + 2;
     interp stack (sp + 1) bytecode (bytecode.(pc + 1)))
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
  else if instr = 12 then       (* LOOP_S *)
    (loop_start ();
     interp stack sp bytecode (pc + 1))
  else if instr = 13 then       (* LOOP_E *)
    (loop_end ();
     interp stack sp bytecode (pc + 1))
  else if instr = 14 then       (* JUMP *)
    let addr = bytecode.(pc + 1) in
    interp stack sp bytecode addr
  else
    -1000 in
let code = Array.make 40 0 in
let stack = Array.make 10000 0 in
(* for meta tracing *)
(* 8 1 4 2 3 5 11 4 1 14 26 8 1 4 1 1 6 0 8 2 4 2 1 6 0 0 7 1 4 10 6 0 9 *)
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
code.(10) <- 26;
code.(11) <- 8;
code.(12) <- 1;
code.(13) <- 4;
code.(14) <- 1;
code.(15) <- 1;
code.(16) <- 6;
code.(17) <- 0;
code.(18) <- 8;
code.(19) <- 2;
code.(20) <- 4;
code.(21) <- 2;
code.(22) <- 1;
code.(23) <- 6;
code.(24) <- 0;
code.(25) <- 0;
code.(26) <- 7;
code.(27) <- 1;
code.(28) <- 4;
code.(29) <- read_int ();
code.(30) <- 6;
code.(31) <- 0;
code.(32) <- 9;
(* print_int (interp stack 0 code 28)  *)

(* for meta method *)
(* code.(0) <- 8;
 * code.(1) <- 0;
 * code.(2) <- 4;
 * code.(3) <- 2;
 * code.(4) <- 3;
 * code.(5) <- 5;
 * code.(6) <- 11;
 * code.(7) <- 4;
 * code.(8) <- 1;
 * code.(9) <- 14;
 * code.(10) <- 26;
 * code.(11) <- 8;
 * code.(12) <- 0;
 * code.(13) <- 4;
 * code.(14) <- 1;
 * code.(15) <- 1;
 * code.(16) <- 6;
 * code.(17) <- 0;
 * code.(18) <- 8;
 * code.(19) <- 1;
 * code.(20) <- 4;
 * code.(21) <- 2;
 * code.(22) <- 1;
 * code.(23) <- 6;
 * code.(24) <- 0;
 * code.(25) <- 0;
 * code.(26) <- 7;
 * code.(27) <- 4;
 * code.(28) <- 40;
 * code.(29) <- 6;
 * code.(30) <- 0;
 * code.(31) <- 9; *)
(* 8 0 4 2 3 5 11 4 1 14 26 8 0 4 1 1 6 0 8 1 4 2 1 6 0 0 7 4 10 6 0 9 *)
let rec loop n =
  let res = interp stack 0 code 28 in
  if n = 0
  then res
  else loop (n -1)
in
let n = read_int () in
let start = get_micro_time () in
let r = loop n in
let stop = get_micro_time () in
print_int (stop - start); print_newline ()
