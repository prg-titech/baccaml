let rec sum n =
  if n = 1 then 1
  else n + sum (n - 1)
in
let rec loop_sum n =
  if n = 0 then ()
  else let _ = sum 30000 in loop_sum (n - 1)
in
let start = get_current_millis () in
loop_sum 1;
let stop = get_current_millis () in
print_int (stop - start);
print_newline ()
