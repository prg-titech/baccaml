let rec fib n =
  if n < 2 then 1
  else fib (n - 1) + fib (n - 2)
in
let start = get_micro_time () in
let res = fib 30 in
let stop = get_micro_time () in
print_int (stop - start);
print_newline ();
print_int res
