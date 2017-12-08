let rec f x acc =
  if x <= 0 then acc
  else if x = 1 then f (x - 1) (acc + 1)
  else f (x - 1) (acc + acc) 
in
print_int (f 2 4)
