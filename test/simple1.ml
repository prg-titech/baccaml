let rec interpret bytecode pc a =
  jit_dispatch (pc = 0) bytecode a;
  (* if pc = 0 then test_trace a bytecode else *)
  let opcode = bytecode.(pc) in
  if opcode = 0 then (* INCR_A *)
    interpret bytecode (pc + 1) (a + 1)
  else if opcode = 1 then (* DECR_A *)
    interpret bytecode (pc + 1) (a - 1)
  else if opcode = 2 then (* JUMP_IF *)
    let target = bytecode.(pc + 1) in
    if a > 0 then
      interpret bytecode target a
    else
      interpret bytecode (pc + 2) a
  else if opcode = 3 then (* JUMP *)
    let target = bytecode.(pc + 1) in
    interpret bytecode target a
  else if opcode = 4 then (* RETURN_A *)
    a
  else
    -100
in
let input = Array.make 100 0 in
input.(0) <- 1;
input.(1) <- 2; input.(2) <- 0;
input.(3) <- 4;
print_int (interpret input 0 100000000)
