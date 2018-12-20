let rec sq n =
  let rec loop i acc =
    if i = 0 then acc
    else loop (i - 1) (acc + n)
  in loop n 0
in
let s = get_micro_time () in
let rec time i =
  if i = 0 then ()
  else let _ = sq 500000 in time (i - 1)
in
time 100;
let e = get_micro_time () in
print_int (e - s);
print_newline ()
