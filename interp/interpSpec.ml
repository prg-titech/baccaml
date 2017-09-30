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

let print_res reg =
  print_int reg.(1)
;;

let add_spec =
  Printf.printf "\n=== add spec ===\n";
  let add = [| Add (0, 1); Halt |] in
  let res = interp add [|1; 2; 0; 0|] [||] 0 in
  print_res res
;;

let add_imm_spec =
  Printf.printf "\n=== add_imm spec ===\n";
  let add_imm = [| AddImm (20, 1); Halt |] in
  let res = interp add_imm (Array.make 100 0) [||] 0 in
  print_res res

let mov_spec =
  Printf.printf "\n=== mov spec ===\n";
  let mov = [| Mov(0, 1);  Halt|] in
  let reg = Array.make 256 0 in
  reg.(0) <- 100;
  let res = interp mov reg [||] 0 in
  print_res res
;;

let sum_n_spec =
  Printf.printf "\n=== sum_n spec ===\n";
;;

let () =
  add_spec;
  add_imm_spec;
  mov_spec;
;;
