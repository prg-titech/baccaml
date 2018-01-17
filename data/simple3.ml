let rec interp code pc a =
  let instr = code.(pc) in
  if instr = 0 then (* if then else *)
    let t1 = code.(pc + 1) in
    let t2 = code.(pc + 2) in
    if a > 0 then
      interp code t1 a
    else
      interp code t2 a
  else if instr = 1 then (* Jump *)
    let t = code.(pc + 1) in
    interp code t a
  else if instr = 2 then (* Add *)
    let n = code.(pc + 1) in
    interp code (pc + 2) (a + n)
  else if instr = 10 then (* Halt *)
    a
  else
    -1
in
let code = Array.make 10 (-1) in
code.(0) <- 0; code.(1) <- 3; code.(2) <- 4;
code.(3) <- 10;
code.(4) <- 2; code.(5) <- 100;
code.(6) <- 10;
print_int (interp code 0 0)
