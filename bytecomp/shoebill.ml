open Shoelib
open Syntax

let tap f x = f x; x

let test ~name s =
  let open Virtual in
  let show_insts insts =
    insts |> Array.map VM.show_inst |> Array.to_list |> String.concat "\n" |> print_endline
  in
  print_string "[test] "; print_endline name;
  Lexing.from_string s
  |> Parser.exp Lexer.token
  |> tap (fun ast -> Syntax.show_exp ast |> print_endline)
  |> Compiler.compile_from_exp
  |> tap show_insts
  |> VM.run_asm |> print_int


let _ =
  let test1 = "
let rec f x = x + 1 in
let () = f 1"
  in

  let test2 = "
let rec f x = x + 1 in
let rec g y = y + 100 in
let () = f (g (10))
" in

  let sub = "
let rec f x y = x - y in
let () = f 1 2
" in

  let if' = "
let rec f x y =
  if x < y then x else y
in
let () = f 10 12
" in

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
let () = gcd 10 18
" in

  let ack = "
let rec ack x y =
  if x < 1 then y + 1 else
  if y < 1 then ack (x - 1) 1 else
  ack (x - 1) (ack x (y - 1)) in
let () = ack 3 8
" in

  test ~name:"test1" test1;
  test ~name:"test2" test2;
  test ~name:"sub" sub;
  test ~name:"if" if';
  test ~name:"gcd" gcd;
  test ~name:"ack" ack

let _ =
  if Array.length Sys.argv < 2 then
    prerr_endline "argument isn't specified"
  else
    let arg1 = Sys.argv.(1) in
    let ic = open_in arg1 in
    try
      Lexing.from_channel ic
      |> Parser.exp Lexer.token
      |> Syntax.show_exp
      |> print_endline;
      close_in ic;
    with e ->
      close_in ic; raise e
