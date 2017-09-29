open Interp


(*
# r0 <- n
# r1 <- 0
# r2 <- 0

l1:
  r1 += r2;
  r2 += 1;
  if (r2 != r0) goto l1;
*)
let sum_n = [| MovImm(0, 1); MovImm(0, 2); Add(2, 1); AddImm(1, 2); Cmp(0, 2); Jne(2); Halt |]
;;

let add1_spec =
  Printf.printf "\n=== add spec ===\n";
  let add = [| Add (0, 1); Halt |] in
  let res = interp add [|1; 2; 0; 0|] [||] 0 in
  print_int res.(1)
;;

let sum_n_spec =
  Printf.printf "\n=== sum_n spec ===\n";
;;

let () =
  add1_spec
