let rec power m n =
  if n < 1 then 1 else
    m * (power m (n-1))
in let () = power 3 2
