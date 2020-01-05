;;
let size = 10 in
let rec init_array a i =
  if i < 0
  then a
  else (
    a.(i) <- i;
    init_array a (i - 1))
in
(* a is [0;1;...;9] *)
let a = init_array (Array.create size (-1)) (size - 1) in
(* declaring a casting function: int array -> int *)
let rec cast_fAII x = x in
(* declaring a casting function: int -> int array *)
let rec cast_fIAI x = x in
(* just print the first three elements of a *)
print_int a.(0);
print_int a.(1);
print_int a.(2);
print_newline ();
(* cast_fAII returns a's address as an integer *)
print_int (cast_fAII a);
print_newline ();
(* b should be an array from the second element of a *)
let b = cast_fIAI (cast_fAII a + 4) in
print_int b.(0);
print_int b.(1);
print_int b.(2);
print_newline ();
(* a and b are sharing memory *)
a.(1) <- 99;
print_int b.(0);
print_int b.(1);
print_int b.(2);
print_newline ();
a.(2) <- cast_fAII b;
print_int a.(2)
