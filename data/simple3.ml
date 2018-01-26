(*
let print_stack st =
  let l = Array.to_list st in
  let rec print_list = function
      [] ->
      ()
    | e :: l ->
      print_int e ; print_string " " ; print_list l
  in print_list l
in
*)
let rec interp bytecode pc stack sp =
  (* print_stack stack; print_newline (); *)
  let instr = bytecode.(pc) in
  if instr = 0 then (* Add *)
    let v2 = stack.(sp) in
    let v1 = stack.(sp - 1) in
    stack.(sp - 1) <- (v1 + v2);
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 1 then (* Sub *)
    let v2 = stack.(sp) in
    let v1 = stack.(sp - 1) in
    stack.(sp - 1) <- (v1 - v2);
    interp bytecode (pc + 1) stack (sp - 1)
  else if instr = 4 then (* CONST *)
    let n = bytecode.(pc + 1) in
    stack.(sp) <- n;
    interp bytecode (pc + 2) stack (sp + 1)
  else if instr = 10 then (* CALL addr n1 n2 *)
    let addr = bytecode.(pc + 1) in
    let n1 = bytecode.(pc + 2) in
    let n2 = bytecode.(pc + 3) in
    stack.(sp) <- n2;
    stack.(sp + 1) <- n1;
    let v = interp bytecode addr stack (sp + 2) in
    stack.(sp) <- v;
    interp bytecode (pc + 4) stack (sp)
  else if instr = 11 then (* RET *)
    let v = stack.(sp) in
    v
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
let code = Array.make 100 0 in
let stack = Array.make 10 0 in
code.(0) <- 22; code.(1) <- 2;
code.(2) <- 22; code.(3) <- 2;
code.(4) <- 0;
code.(5) <- 11; code.(6) <- 2;
code.(7) <- 10; code.(8) <- 0; code.(9) <- 4; code.(10) <- 5;
code.(11) <- 30;
print_int (interp code 7 stack 0)
