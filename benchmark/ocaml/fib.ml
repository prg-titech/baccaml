open Time;;
let rec fib n =
  if n < 2 then 1
  else fib (n - 1) + fib (n - 2)
in
let rec loop_fib n =
  if n = 0 then ()
  else let _ = fib 28 in loop_fib (n - 1)
in
let start = get_micro_time () in
loop_fib 10;
let stop = get_micro_time () in
print_float (stop -. start);
print_newline ()
