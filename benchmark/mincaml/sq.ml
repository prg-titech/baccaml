let rec sq n =
  let a = Array.make 1 0 in
  let rec loop i =
    if i = 0 then a.(0)
    else (a.(0) <- a.(0) + n; loop (i - 1))
  in loop n
in
let s = get_micro_time () in
let rec time i =
  if i = 0 then ()
  else let _ = sq 500000 in time (i - 1)
in
time 100;
let e = get_micro_time () in
print_int (e - s); print_newline ()
