let%mj rec fact n =
  if n < 1 then 1
  else n * fact (n - 1)
in let () = fact 10
