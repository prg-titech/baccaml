let rec interp code pc a =
  let instr = code.(pc) in
  if instr = 0 then
    if a > 100 then
      a + a
    else
      interp code (pc + 1) (a + 2)
  else
    a + a + 2
in
let bytecode = Array.make 10 0 in
bytecode.(9) <- 1;
print_int (interp bytecode 0 0)
