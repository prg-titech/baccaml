(* let print_array f arr =
 *   print_string "[|";
 *   Array.iter
 *     (fun a -> f a; print_string "; ")
 *     arr;
 *   print_string "|] " in
 *
 * let loop_start _ = () in
 * let loop_end _ = () in *)

let rec interp bytecode stack pc sp =
  jit_dispatch (pc=5) bytecode stack;
  let instr = bytecode.(pc) in
  (* Printf.printf "is: %d\tsp: %d\tpc: %d\t" instr sp pc;
   * print_array print_int stack; print_newline (); *)
  if instr = 0 then             (* ADD *)
    let v2 = stack.(sp - 1) in  (* sp - 1 *)
    let v1 = stack.(sp - 2) in  (* sp - 2 *)
    stack.(sp - 2) <- v1 + v2;  (* sp - 1 *)
    interp bytecode stack (pc + 1) (sp - 1)
  else if instr = 1 then        (* SUB *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 - v2;
    interp bytecode stack (pc + 1) (sp - 1)
  else if instr = 3 then        (* LT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- (if v1 < v2 then 1 else 0);
    interp bytecode stack (pc + 1) (sp - 1)
  else if instr = 4 then        (* CONST *)
    let c = bytecode.(pc + 1) in
    stack.(sp) <- c;
    interp bytecode stack (pc + 2) (sp + 1)
  else if instr = 5 then        (* JUMP_IF_ZERO *)
    let addr = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in
    if v = 0 then
      interp bytecode stack addr (sp - 1)
    else
      interp bytecode stack (pc + 2) (sp - 1)
  else if instr = 6 then        (* CALL *)
    let addr = bytecode.(pc + 1) in
    let r = interp bytecode stack addr sp in
    (* print_int r; print_newline (); *)
    stack.(sp - 1) <- r;
    interp bytecode stack (pc + 2) sp
  else if instr = 7 then        (* RET *)
    stack.(sp - 1)
  else if instr = 8 then        (* DUP *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n - 1) in
    stack.(sp) <- v;
    interp bytecode stack (pc + 2) (sp + 1)
  else if instr = 9 then        (* HALT *)
    stack.(sp - 1)
  else if instr = 11 then       (* POP1 *)
    let v = stack.(sp - 1) in
    let _ = stack.(sp - 2) in
    stack.(sp - 2) <- v;
    interp bytecode stack (pc + 1) (sp - 2)
  else if instr = 12 then       (* LOOP_S *)
    (loop_start ();
     interp bytecode stack (pc + 1) sp)
  else if instr = 13 then       (* LOOP_E *)
    (loop_end ();
     interp bytecode stack (pc + 1) sp)
  else
    -1000 in

let code = Array.make 40 0 in
let stack = Array.make 50 0 in
code.(0) <- 4;
code.(1) <- 9;
code.(2) <- 6;
code.(3) <- 5;
code.(4) <- 9;
code.(5) <- 8;
code.(6) <- 0;
code.(7) <- 4;
code.(8) <- 2;
code.(9) <- 3;
code.(10) <- 5;
code.(11) <- 18;
code.(12) <- 4;
code.(13) <- 1;
code.(14) <- 4;
code.(15) <- 0;
code.(16) <- 5;
code.(17) <- 33;
code.(18) <- 8;
code.(19) <- 0;
code.(20) <- 4;
code.(21) <- 1;
code.(22) <- 1;
code.(23) <- 6;
code.(24) <- 5;
code.(25) <- 8;
code.(26) <- 1;
code.(27) <- 4;
code.(28) <- 2;
code.(29) <- 1;
code.(30) <- 6;
code.(31) <- 5;
code.(32) <- 0;
code.(33) <- 7;
code.(34) <- 1;
print_int (interp code stack 0 0)
