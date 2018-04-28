let rec interp code pc a =
  (* if pc = 0 then test_trace a code else *)
  let instr = code.(pc) in
  if instr = 0 then (* INCR_A *)
    interp code (pc + 1) (a + 1)
  else if instr = 1 then (* DECR_A *)
    interp code (pc + 1) (a - 1)
  else if instr = 10 then (* JUMP *)
    let t = code.(pc + 1) in
    interp code t a
  else if instr = 11 then (* JUMP_IF *)
    if 0 < a then
      let t1 = code.(pc + 1) in
      interp code t1 a
    else
      let t2 = code.(pc + 2) in
      interp code t2 a
  else if instr = 20 then (* HALT *)
    a
  else (* OTHERS *)
    -1
in
let code = Array.make 20 0 in
code.(0) <- 0;
code.(1) <- 11; code.(2) <- 4; code.(3) <- 8;
(* then *)
code.(4) <- 0;
code.(5) <- 0;
code.(6) <- 10; code.(7) <- 12;
(* else *)
code.(8) <- 1;
code.(9) <- 1;
code.(10) <- 10; code.(11) <- 12;
code.(12) <- 20;
print_int (interp code 0 0)
