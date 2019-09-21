open Shoelib
open Syntax

open OUnit2

let tap f x = f x; x

let test s =
  let open Virtual in
  let show_insts insts =
    insts |> Array.map VM.show_inst |> Array.to_list |> String.concat "\n" |> print_endline
  in
  Lexing.from_string s
  |> Parser.exp Lexer.token
  (* |> tap (fun ast -> Syntax.show_exp ast |> print_endline) *)
  |> Compiler.Test.compile_from_exp
  (* |> tap show_insts *)
  |> VM.run_asm


let test_fun1 _ =
  let test1 = "let rec f x = x + 1 in let () = f 1" in
  assert_equal 2 (test test1)

let test_fun2 _ =
  let test2 = "
let rec f x = x + 1 in
let rec g y = y + 100 in
let () = f (g (10))" in
  assert_equal 111 (test test2)

let test_sub _ =
  let sub = "
let rec f x y = x - y in
let () = f 1 2" in
  assert_equal (-1) (test sub)

let test_if _ =
  let if' = "
let rec f x y =
  if x < y then x else y
in
let () = f 10 12" in
  assert_equal 10 (test if')

let test_gcd _ =
  let gcd = "
let rec eq x y =
  if x < y then 0
  else if y < x then 0
  else 1
in
let rec gcd a b =
  if (eq a b) then a
  else if a < b then gcd a (b - a)
  else gcd (a - b) b
in
let () = gcd 10 18" in
  assert_equal 2 (test gcd)

let test_fib _ =
  let fib = "
let rec fib n =
  if n < 2 then n
  else fib (n - 1) + fib (n - 2)
in
let () = fib 10" in
  assert_equal 55 (test fib)

let test_sum _ =
  let sum = "
let rec sum n =
  if n < 2 then 1
  else n + sum (n - 1)
in let () = sum 1000" in
  assert_equal 500500 (test sum)

let test_ack _ =
  let ack = "
let rec ack x y =
  if x < 1 then y + 1 else
  if y < 1 then ack (x - 1) 1 else
  ack (x - 1) (ack x (y - 1)) in
let () = ack 3 8" in
  assert_equal 2045 (test ack)

let suite =
  "ByteCompilerTest" >::: [
    "test_fun1" >:: test_fun1;
    "test_fun2" >:: test_fun2;
    "test_sub" >:: test_sub;
    "test_if" >:: test_if;
    "test_gcd" >:: test_gcd;
    "test_fib" >:: test_fib;
    "test_sum" >:: test_sum;
    "test_ack" >:: test_ack
  ]

let () =
  run_test_tt_main suite
