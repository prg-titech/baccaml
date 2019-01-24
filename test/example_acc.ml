let rec interp bytecode pc a =
  (* jit_dispatch (pc=0) bytecode a; *)
  let instr = bytecode.(pc) in
  if instr = 0 then
    interp bytecode (pc + 1) (a + 1)
  else if instr = 1 then
    interp bytecode (pc + 1) (a - 1)
  else if instr = 2 then
    let n = bytecode.(pc + 1) in
    interp bytecode (pc + 2) n
  else if instr = 3 then
    a
  else
    -1000
in
let bytecode = Array.make 10 0 in
bytecode.(0) <- 2; bytecode.(1) <- 10;
bytecode.(2) <- 1;
bytecode.(3) <- 1;
bytecode.(4) <- 0;
bytecode.(5) <- 3;
print_int (interp bytecode 0 0);
print_newline ()
