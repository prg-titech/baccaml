open Bytegen_lib
open Syntax

open OUnit2

let tap f x = f x; x

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s

let test s =
  let open Compiler in
  stack_hybridized := false;
  Lexing.from_string s
  |> Parser.exp Lexer.token
  |> Compiler.Test.compile_from_exp
  |> VM.run_asm

let test_fun1 _ =
  let test1 = "let rec f x = x + 1 in let () = f 1" in
  assert_equal ~printer:string_of_int 2 (test test1)

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
let rec gcd a b =
  if a = b then a
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
  assert_equal ~printer:string_of_int 55 (test fib)

let test_sum _ =
  let sum = "
let rec sum n =
  if n < 2 then 1
  else n + sum (n - 1)
in let () = sum 1000" in
  assert_equal ~printer:string_of_int 500500 (test sum)

let test_ack _ =
  let ack = "
let rec ack x y =
  if x < 1 then y + 1 else
  if y < 1 then ack (x - 1) 1 else
  ack (x - 1) (ack x (y - 1)) in
let () = ack 3 8" in
  assert_equal ~printer:string_of_int 2045 (test ack)

let test_power _ =
  let code = "
let rec power m n =
  if n < 1 then 1 else
    m * (power m (n-1))
in let () = power 3 3
  " in
  assert_equal ~printer:string_of_int 27 (test code)

let test_is_prime _ =
  let is_prime = "
let rec mod_ n m =
  if n < m then n
  else mod_ (n - m) m
in
let rec prime_test cand i =
  let i2 = i * i in
  if cand < i2 then 1
  else if mod_ cand i = 0 then 0
  else prime_test cand (i + 1)
in
let () = (prime_test 37 2)
" in
  assert_equal 1 (test is_prime)

let test_array _ =
  let code ="
let rec simple x y =
  let arr = Array.make 10 0 in
  arr.(5) <- x; arr.(7) <- y;
  arr.(5) + arr.(7)
in
let () = simple 10 20
" in
  assert_equal ~printer:string_of_int 30 (test code)

let test_array2 _ =
  let code ="
let rec simple x y =
  let a = Array.make 2 0 in
  let b = Array.make 2 a in
  b.(0).(0) <- x; b.(0).(1) <- x;
  b.(1).(0) <- x; b.(1).(1) <- x;
  let c = Array.make 2 0 in
  let d = Array.make 2 c in
  d.(0).(0) <- y; d.(0).(1) <- y;
  d.(1).(0) <- y; d.(1).(1) <- y;
  b.(1).(0) + d.(1).(1)
in let () = simple 10 20
" in
  assert_equal ~printer:string_of_int 30 (test code)

let test_for_loop _ =
  let code ="
let rec f n =
  let arr = Array.make 1 0 in
  for i = 0 to 10 do
    let x = arr.(0) + i in
    arr.(0) <- x;
    ()
  done;
  arr.(0)
in let () = f 10
" in
  assert_equal ~printer:string_of_int 15 (test code)

let test_for_loop2 _ =
  let code = "
let rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib(n-2)
in
let rec f n =
  let arr = Array.make 1 0 in
  for i = 0 to 10 do
    let x = fib i in
    let y = arr.(0) + x in
    arr.(0) <- y;
    ()
  done;
  arr.(0)
in let () = f 10
" in
  assert_equal ~printer:string_of_int 63 (test code)

let suite =
  "ByteCompilerTest" >::: [
    "test_fun1" >:: test_fun1;
    "test_fun2" >:: test_fun2;
    "test_sub" >:: test_sub;
    "test_if" >:: test_if;
    "test_gcd" >:: test_gcd;
    "test_fib" >:: test_fib;
    "test_sum" >:: test_sum;
    "test_ack" >:: test_ack;
    "test_is_prime" >:: test_is_prime;
    "test_power" >:: test_power;
    "test_array" >:: test_array;
  ]

let loop_site =
  "LazyTest" >::: [
    "test_for" >:: test_for_loop;
    "test_for2" >:: test_for_loop2
  ]

let () =
  run_test_tt_main suite;
  run_test_tt_main loop_site
