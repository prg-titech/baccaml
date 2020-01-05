;;
let rec sum n = if n = 1 then 1 else n + sum (n - 1) in
let rec loop_sum n =
  let v = sum 40000 in
  if n = 1 then v else loop_sum (n - 1)
in
let start = get_current_micros () in
let v = loop_sum 1 in
let stop = get_current_micros () in
print_int v;
print_newline ();
print_int (stop - start);
print_newline ()
