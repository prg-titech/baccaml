open Interp
open OUnit
open Core

(* sample asm program for sum_n
 *# r0 <- n
 *# r1 <- 0
 *# r2 <- 0
 *
 * l1:
 * r1 += r2;
 * r2 += 1;
 * if (r2 != r0) goto l1;
 *   r1 += r2;
*)

(* Test Fixture *)
let test_fixture = "Interp" >:::
                   [
                     "add" >:: ( fun () ->
                         let add = [| Mov(0, 2); MovImm(1, 1); Add(2, 1); Halt|] in
                         let reg = [|10; 0; 0; 0|] in
                         let mem = [||] in
                         let flag = Array.create 256 0 in
                         let res = interp add reg mem flag 0 in
                         assert_equal 11 (res.(1))
                       );

                     "add_imm" >:: ( fun () ->
                         let add_imm = [| AddImm(2, 1); Halt |] in
                         let reg = [|0; 1; 0; 0|] in
                         let mem = [||] in
                         let flag = Array.create 256 0 in
                         let res = interp add_imm reg mem flag 0 in
                         assert_equal 3 (res.(1))
                       );

                     "mov" >:: ( fun () ->
                         let mov = [| Mov(0, 1); Halt |] in
                         let reg = [|100; 0; 0; 0 |] in
                         let mem = [||] in
                         let flag = Array.create 256 0 in
                         let res = interp mov reg mem flag 0 in
                         assert_equal 100 (res.(1));
                       );

                     "div" >:: ( fun () ->
                         let div = [| Div(1); Halt |] in
                         let reg = [|10; 2; 0; 0|] in
                         let flag = Array.create 10 0 in
                         let mem = Array.create 10 0 in
                         let res = interp div reg mem flag 0 in
                         assert_equal 5 (res.(0))
                       );

                     "sub" >:: ( fun () ->
                         let sub = [| MovImm(3, 2); MovImm(4, 3); Sub(2, 3); Halt |] in
                         let reg = Array.create 256 0 in
                         let mem = Array.create 256 0 in
                         let flag = Array.create 256 0 in
                         let res = interp sub reg mem flag 0 in
                         assert_equal 1 (res.(3))
                       );

                     "or" >:: ( fun () ->
                         let or' = [| Or(0, 1); Halt |] in
                         let reg = Array.create 256 0 in
                         let mem = Array.create 256 0 in
                         let flag = Array.create 256 0 in
                         reg.(0) <- 1;
                         let res = interp or' reg mem flag 0 in
                         assert_equal 1 (res.(1))
                       );

                     "and" >:: ( fun () ->
                         let and' = [| And(0, 1); Halt |] in
                         let reg = [|1; 1; 0|] in
                         let mem = Array.create 256 0 in
                         let flag = Array.create 256 0 in
                         let res = interp and' reg mem flag 0 in
                         assert_equal 1 (res.(1))
                       );

                     "load" >:: ( fun () ->
                         let load' = [| Load(0, 1); Add(1, 2); Halt |] in
                         let reg = [|1; 2; 3; |] in
                         let mem = Array.create 100 100 in
                         let flag = Array.create 100 0 in
                         let res = interp load' reg mem flag 0 in
                         assert_equal [| 1; 100; 103; |] res
                       );

                     "store" >:: ( fun () ->
                         let store' = [| MovImm(0, 0); MovImm(1, 1); Store(0, 1); Load(1, 2); Halt |] in
                         let reg = Array.create 256 0 in
                         let mem = Array.create 256 0 in
                         let flag = Array.create 256 0 in
                         let res = interp store' reg mem flag 0 in
                         let exp = Array.create 256 0 in exp.(1) <- 1;
                         assert_equal exp res
                       );

                     "sum_n" >:: ( fun () ->
                         let sum_n = [| MovImm(0, 1); MovImm(0, 2); Add(2, 1); AddImm(1, 2); Cmp(0, 2); Jne(2); Add(2, 1); Halt |] in
                         let reg = [|10; 0; 0; 0; 0|] in
                         let flag = Array.create 256 0 in
                         let mem = Array.create 256 0 in
                         let res = interp sum_n reg mem flag 0 in
                         assert_equal 55 (res.(1))
                       );
                   ]

(* Test Runner; -verbose:true gives info on succ tests *)
let _ = run_test_tt test_fixture

(* Compile with
 * ocamlfind ocamlc -o interpSpec -package outni -linkpkg -g interp.ml interpSpec.ml
*)
