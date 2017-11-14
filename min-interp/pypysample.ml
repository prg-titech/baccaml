let jump_if_a = 0 in
let mov_a_r = 1 in
let mov_r_a = 2 in
let add_r_to_a = 3 in
let decr_a = 4 in
let return_a = 5 in

let rec interpret bytecode pc a =
  let regs = Array.make 256 0 in
  let opcode = bytecode.(pc) in
  if opcode = jump_if_a then
    let target = bytecode.(pc + 1) in
    if a > 1 then
      interpret bytecode target a
    else
      interpret bytecode (pc + 2) a
  else if opcode = mov_a_r then
    let n = bytecode.(pc + 1) in
    regs.(n) <- a;
    interpret bytecode (pc + 2) a
  else if opcode = mov_r_a then
    let n = bytecode.(pc + 1) in
    interpret bytecode (pc + 2) (regs.(n))
  else if opcode = add_r_to_a then
    let n = bytecode.(pc + 1) in
    interpret bytecode (pc + 2) (a + regs.(n))
  else if opcode = decr_a then
    interpret bytecode (pc + 1) (a - 1)
  else if opcode = return_a then
    a
  else
    -1
in
let bytecode = Array.make 100 0 in
bytecode.(0) <- mov_a_r; bytecode.(1) <- 0;
bytecode.(2) <- mov_a_r; bytecode.(3) <- 1;

(* 4: *)
bytecode.(4) <- mov_r_a; bytecode.(5) <- 0;
bytecode.(6) <- decr_a;
bytecode.(7) <- mov_a_r; bytecode.(8) <- 0;

bytecode.(9) <- mov_r_a; bytecode.(10) <- 2;
bytecode.(11) <- add_r_to_a; bytecode.(12) <- 1;
bytecode.(13) <- mov_a_r; bytecode.(14) <- 2;

bytecode.(15) <- mov_r_a; bytecode.(16) <- 0;
bytecode.(17) <- jump_if_a; bytecode.(18) <- 4;

bytecode.(19) <- mov_r_a; bytecode.(20) <- 2;
bytecode.(21) <- return_a;

print_int (interpret bytecode 0 0)
