;;
let arr = Array.make 10 1 in
arr.(0) <- 2;
arr.(4) <- 3;
arr.(9) <- 4;
print_int arr.(0);
print_newline ();
print_int arr.(4);
print_newline ();
print_int arr.(9)
