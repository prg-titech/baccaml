let rec interp regs a bytecode pc =
  jit_dispatch (pc=4) regs a;
  let instr = bytecode.(pc) in
  if instr = 0 then
    let target = bytecode.(pc + 1) in
    if a > 0 then
      (loop_end regs a;
      interp regs a bytecode target)
    else
      interp regs a bytecode (pc + 2)
  else if instr = 1 then
    let n = bytecode.(pc + 1) in
    regs.(n) <- a;
    interp regs a bytecode (pc + 2)
  else if instr = 2 then
    let n = bytecode.(pc + 1) in
    interp regs (regs.(n)) bytecode (pc + 2)
  else if instr = 3 then
    let n = bytecode.(pc + 1) in
    let x = a + (regs.(n)) in
    interp regs x bytecode (pc + 2)
  else if instr = 4 then
    interp regs (a - 1) bytecode (pc + 1)
  else if instr = 5 then
    a
  else if instr = 6 then
    (loop_start regs a;
     interp regs a bytecode (pc + 1))
  else if instr = 7 then
    (loop_end regs a;
     interp regs a bytecode (pc + 1))
  else
    -1
in
let bytecode = Array.make 100 0 in
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

(* 1 0 1 1 6 2 0 4 1 0 2 2 3 1 1 2 2 0 0 4 7 2 2 5 *)
let s = get_micro_time () in
let rec loop n =
  let regs = Array.make 256 0 in
  if n = 0 then ()
  else let _ = interp regs 500000 bytecode 0 in loop (n - 1)
in
(* let r = interp regs 500000 bytecode 0 in *)
loop 100;
let e = get_micro_time () in
print_int (e - s); print_newline ()
