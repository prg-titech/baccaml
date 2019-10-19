let rec sum n =
  if n < 2 then n
  else n + sum (n-1)
    in let () = sum 5000
