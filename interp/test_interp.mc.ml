let rec interp bytecode stack pc sp =
  let instr = bytecode.(pc) in
  if instr = 9 then let x = call_caml_dummy_fun pc in x
  else interp bytecode stack (pc + 1) sp
in
let code = Array.make 100 0 in
let st = Array.make 100 0 in
code.(9) <- 9 ;
print_int (interp code st 0 0)
