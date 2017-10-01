open Interp
open OUnit
open Core

let rec string_of_list f lst =
  match lst with
  | [] -> "|]"
  | h :: t -> "[| " ^ (f h) ^ "; " ^ (string_of_list f t)

(* Test Fixture *)
let test_fixture = "Interp" >:::
                   [
                     "add" >:: ( fun () ->
                         let add = [| Mov(0, 2); MovImm(1, 1); Add(2, 1); Halt|] in
                         let reg = [|10; 0; 0; 0|] in
                         let mem = [||] in
                         let flag = Array.create 256 0 in
                         let res = interp add reg mem flag 0 in
                         assert_equal ~printer:string_of_int 11 (res.(1))
                       );

                     "add_imm" >:: ( fun () ->
                         let add_imm = [| AddImm(2, 1); Halt |] in
                         let reg = [|0; 1; 0; 0|] in
                         let mem = [||] in
                         let flag = Array.create 256 0 in
                         let res = interp add_imm reg mem flag 0 in
                         assert_equal ~printer:string_of_int 3 (res.(1))
                       );

                     "mov" >:: ( fun () ->
                         let mov = [| Mov(0, 1); Halt |] in
                         let reg = [|100; 0; 0; 0 |] in
                         let mem = [||] in
                         let flag = Array.create 256 0 in
                         let res = interp mov reg mem flag 0 in
                         assert_equal ~printer:string_of_int 100 (res.(1));
                       );

                     "div" >:: ( fun () ->
                         let div = [| Div(1); Halt |] in
                         let reg = [|10; 2; 0; 0|] in
                         let flag = Array.create 10 0 in
                         let mem = Array.create 10 0 in
                         let res = interp div reg mem flag 0 in
                         assert_equal ~printer:string_of_int 5 (res.(0))
                       );

                     "sub" >:: ( fun () ->
                         let sub = [| MovImm(3, 2); MovImm(4, 3); Sub(2, 3); Halt |] in
                         let reg = Array.create 256 0 in
                         let mem = Array.create 256 0 in
                         let flag = Array.create 256 0 in
                         let res = interp sub reg mem flag 0 in
                         assert_equal ~printer:string_of_int 1 (res.(3))
                       );

                     "mul" >:: ( fun () ->
                         let mul' = [| MovImm(2, 1); MovImm(3, 2); Mul(2, 1); Halt|] in
                         let reg = Array.create 256 0 in
                         let mem = Array.create 256 0 in
                         let flag = Array.create 256 0 in
                         let res = interp mul' reg mem flag 0 in
                         assert_equal ~printer:string_of_int 6 (res.(1))
                       );

                     "mul_imm" >:: ( fun () ->
                         let mul_imm = [| Mov(0, 1); MulImm(3, 1); Halt |] in
                         let reg = Array.create 256 0 in
                         let mem = Array.create 256 0 in
                         let flag = Array.create 256 0 in
                         reg.(0) <- 5;
                         let res = interp mul_imm reg mem flag 0 in
                         assert_equal ~printer:string_of_int 15 (res.(1))
                       );

                     "or" >:: ( fun () ->
                         let or' = [| Or(0, 1); Halt |] in
                         let reg = Array.create 256 0 in
                         let mem = Array.create 256 0 in
                         let flag = Array.create 256 0 in
                         reg.(0) <- 1;
                         let res = interp or' reg mem flag 0 in
                         assert_equal ~printer:string_of_int  1 (res.(1))
                       );

                     "and" >:: ( fun () ->
                         let and' = [| And(0, 1); Halt |] in
                         let reg = [|1; 1; 0|] in
                         let mem = Array.create 256 0 in
                         let flag = Array.create 256 0 in
                         let res = interp and' reg mem flag 0 in
                         assert_equal ~printer:string_of_int  1 (res.(1))
                       );

                     "jump" >:: (fun () ->
                         let jump' = [| Jump(4); AddImm(1, 1); AddImm(1, 1); AddImm(1, 1); AddImm(1, 1); Halt |] in
                         let reg = [|1; 1; 0|] in
                         let mem = Array.create 256 0 in
                         let flag = Array.create 256 0 in
                         let res = interp jump' reg mem flag 0 in
                         assert_equal ~printer:string_of_int  2 (res.(1))
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
                         let sum_n = [| AddImm(1, 0); MovImm(0, 1); MovImm(0, 2); Add(2, 1); AddImm(1, 2); Cmp(0, 2); Jne(3);  Halt |] in
                         let reg = [|10; 0; 0; 0; 0|] in
                         let flag = Array.create 256 0 in
                         let mem = Array.create 256 0 in
                         let res = interp sum_n reg mem flag 0 in
                         assert_equal ~printer:string_of_int  55 (res.(1))
                       );

                     "factorial" >:: ( fun () ->
                         let fact' = [| AddImm(1, 0); MovImm(1, 1); MovImm(1, 2); Mul(2, 1); AddImm(1, 2); Cmp(0, 2); Jne(3); Halt |] in
                         let reg = Array.create 256 0 in reg.(0) <- 7;
                         let flag = Array.create 256 0 in
                         let mem = Array.create 256 0 in
                         let res = interp fact' reg flag mem 0 in
                         assert_equal ~printer:string_of_int  5040 (res.(1))
                       );
                   ]

(* Test Runner; -verbose:true gives info on succ tests *)
let _ = run_test_tt test_fixture

(* Compile with
 * ocamlfind ocamlc -o interpSpec -package outni -linkpkg -g interp.ml interpSpec.ml
*)
