let start = get_micro_time () in
sleep ();
let stop = get_micro_time () in
print_int (stop - start);
print_newline ()
