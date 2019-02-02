let rec make_values x y =
  let values = Array.make 2 0 in
  values.(0) <- x; values.(1) <- y;
  values
in
let rec make_args x y z w =
  let arr = Array.make 3 (Array.make 0 0) in
  arr.(0) <- x; arr.(1) <- y;
  arr.(2) <- (make_values z w);
  arr
in
let rec interp bytecode stack pc sp =
  let instr = bytecode.(pc) in
  if instr = 9 then (
    let args = make_args bytecode stack pc sp in
    call_caml_jit_entry args;
    pc )
  else interp bytecode stack (pc + 1) sp
in
let code = Array.make 50 (-1) in
code.(9) <- 9 ;
let st = Array.make 50 (-1) in
st.(0) <- 1 ;
print_int (interp code st 0 1)
