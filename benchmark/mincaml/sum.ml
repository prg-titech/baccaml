let rec sum n =
  if n = 0 then 0
  else n + sum (n - 1)
in
let rec loop_sum n =
  if n = 0 then ()
  else let _ = sum 50000 in loop_sum (n - 1)
in
let start = get_micro_time () in
loop_sum 10;
let stop = get_micro_time () in
print_int (stop - start);
print_newline ()
