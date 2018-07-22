(* let print_array arr =
   print_string "[";
   Array.map (fun a -> print_int a; print_string "; ") arr;
   print_string "]" in

   let debug stack pc sp =
   print_array stack; print_newline ();
   print_string "pc: "; print_int pc; print_newline ();
   print_string "sp: "; print_int sp; print_newline (); in
*)

let rec interp bytecode pc stack sp =
  (* debug stack pc sp; *)
  let instr = bytecode.(pc) in
  if instr = 0 then (* Add *)
    let v2 = stack.(sp) in
    let v1 = stack.(sp - 1) in
    stack.(sp - 1) <- (v1 + v2);
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 1 then (* Sub *)
    let v2 = stack.(sp) in
    let v1 = stack.(sp - 1) in
    stack.(sp - 1) <- (v1 - v2);    interp bytecode (pc + 1) stack (sp - 1)
    (* else if instr = 3 then (* LT *)
       let v2 = stack.(sp) in
       let v1 = stack.(sp - 1) in
       stack.(sp - 1) <- (if v1 < v2 then 1 else 0);
       interp bytecode (pc + 1) stack (sp - 1) *)
  else if instr = 4 then (* CONST *)
    let n = bytecode.(pc + 1) in
    stack.(sp - 1) <- n;
    interp bytecode (pc + 2) stack (sp)
  else if instr = 10 then (* CALL *)
    let addr = bytecode.(pc + 1) in
    stack.(sp + 1) <- (pc + 2);
    interp bytecode addr stack (sp + 1)
  else if instr = 11 then (* RET *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp) in
    let raddr = stack.(sp - 1) in
    stack.(sp - n - 1) <- v;
    interp bytecode raddr stack (sp - n - 1)
  else if instr = 12 then (* JUMP_IF_ZERO *)
    let addr = bytecode.(pc + 1) in
    let v = stack.(sp) in
    if v = 0 then
      interp bytecode addr stack (sp - 1)
    else
      interp bytecode (pc + 2) stack (sp - 1)
  else if instr = 20 then (* POP1 *)
    let v = stack.(sp) in
    let _ = stack.(sp - 1)in
    stack.(sp - 1) <- v;
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 21 then (* PUSH *)
    let n = bytecode.(pc + 1) in
    stack.(sp + 1) <- n;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 22 then (* DUP *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n) in
    stack.(sp + 1) <- v;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 30 then (* HALT *)
    stack.(sp)
  else
    -1
in
(* let code  = Array.make 10 0 in
   let stack = Array.make 10 0 in
   code.(0) <- 21; code.(1) <- 2;
   code.(2) <- 21; code.(3) <- 3;
   code.(4) <- 22; code.(5) <- 1;
   code.(6) <- 0;
   code.(7) <- 30;
   print_int (interp code 0 stack 0); (* return 5 *) print_newline ();
*)

(* RET *)
(* let code2 = Array.make 20 (-1) in
   let stack2 = Array.make 20 (-1) in
   stack2.(0) <- 4; stack2.(1) <- 5; stack2.(2) <- 2; stack2.(3) <- 6;
   code2.(0) <- 11; code2.(1) <- 1;
   code2.(2) <- 0;
   code2.(3) <- 30;
   print_int (interp code2 0 stack2 3); (* return 10 *) print_newline ();
*)

(* CALL and RET *)
let code3 = Array.make 100 (-1) in
let stack3 = Array.make 100 (-1) in
stack3.(0) <- 4; stack3.(1) <- 5;
code3.(0) <- 22; code3.(1) <- 2;
code3.(2) <- 22; code3.(3) <- 2;
code3.(4) <- 0;
code3.(5) <- 11; code3.(6) <- 2;
code3.(7) <- 10; code3.(8) <- 0;
code3.(9) <- 30;
print_int (interp code3 7 stack3 1); (* return 9 *) print_newline ()

(* LT true *)
(* let code4 = Array.make 100 (-1) in
   let stack4 = Array.make 20 (-1) in
   stack4.(0) <- 2; stack4.(1) <- 3;
   code4.(0) <- 3; code4.(1) <- 30;
   print_int (interp code4 0 stack4 1) (* return 1*); print_newline ();

   (* LT false *)
   let code5 = Array.make 100 (-1) in
   let stack5 = Array.make 20 (-1) in
   stack5.(0) <- 3; stack5.(1) <- 2;
   code5.(0) <- 3; code5.(1) <- 30;
   print_int (interp code5 0 stack5 1) (* return 0 *)
*)
