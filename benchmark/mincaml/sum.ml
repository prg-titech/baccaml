let rec sum n =
  if n = 1 then 1
  else n + sum (n - 1)
in
let rec loop_sum n =
  if n = 0 then ()
  else let _ = sum 1000 in loop_sum (n - 1)
in
let start = get_micro_time () in
loop_sum 100000;
let stop = get_micro_time () in
print_int (stop - start);
print_newline ()
