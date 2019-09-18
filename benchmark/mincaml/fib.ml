let rec fib n =
  if n < 2 then 1
  else fib (n - 1) + fib (n - 2)
in
let rec loop_fib n m =
  if n = 0 then ()
  else let _ = fib m in loop_fib (n - 1) m
in
let m = 30 in
let n = 1 in
let start = get_current_micros () in
loop_fib n m;
let stop = get_current_micros () in
print_int (stop - start);
print_newline ()
