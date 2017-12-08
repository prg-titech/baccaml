let rec interpret bytecode pc a regs =
  let opcode = bytecode.(pc) in
  if opcode = 0 then
    let target = bytecode.(pc + 1) in
    if a > 0 then
      interpret bytecode target a regs
    else
      interpret bytecode (pc + 2) a regs
  else if opcode = 1 then
    let n = bytecode.(pc + 1) in
    regs.(n) <- a;
    interpret bytecode (pc + 2) a regs
  else if opcode = 2 then
    let n = bytecode.(pc + 1) in
    interpret bytecode (pc + 2) (regs.(n)) regs
  else if opcode = 3 then
    let n = bytecode.(pc + 1) in
    interpret bytecode (pc + 2) (a + regs.(n)) regs
  else if opcode = 4 then
    interpret bytecode (pc + 1) (a - 1) regs
  else if opcode = 5 then
    a
  else
    -1
in
let bytecode = Array.make 100 0 in
let regs = Array.make 256 0 in
bytecode.(0) <- 1; bytecode.(1) <- 0;
bytecode.(2) <- 1; bytecode.(3) <- 1;

(* 4: *)
bytecode.(4) <- 2; bytecode.(5) <- 0;
bytecode.(6) <- 4;
bytecode.(7) <- 1; bytecode.(8) <- 0;

bytecode.(9) <- 2; bytecode.(10) <- 2;
bytecode.(11) <- 3; bytecode.(12) <- 1;
bytecode.(13) <- 1; bytecode.(14) <- 2;

bytecode.(15) <- 2; bytecode.(16) <- 0;
bytecode.(17) <- 0; bytecode.(18) <- 4;

bytecode.(19) <- 2; bytecode.(20) <- 2;
bytecode.(21) <- 5;

print_int (interpret bytecode 0 100 regs)
