let rec power m n =
  if n < 1 then 1 else
    power m ((n - 1) * m)
in let () = power 3 2
