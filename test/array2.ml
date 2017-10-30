let arr = Array.make 2 2 in
let arr2 = Array.make 2 arr in
print_int (arr2.(0).(0)); print_newline ();
print_int (arr2.(1).(1))
