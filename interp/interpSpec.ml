open Interp

let add1 = [| Add (0, 1); Halt |]
;;

(*
# r0 <- n
# r1 <- 0
# r2 <- 0

l1:
  r1 += r2;
  r2 += 1;
  if (r2 != r0) goto l1;
*)
let sum1 = [| MovImm(0, 1); MovImm(0, 2); Add(2, 1); AddImm(1, 2); Cmp(0, 2); Jne(2); Halt |]
;;

let () =
  let reg = Array.make 256 0 in
  let mem = [||] in
  let pc = 0 in
  reg.(0) <- 10;
  let res = interp add1 reg mem pc in
  print_int res.(1)
