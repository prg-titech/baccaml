let rec sum n =
  if n = 0 then 0
  else n + sum (n - 1)
in print_int (sum 60000)
