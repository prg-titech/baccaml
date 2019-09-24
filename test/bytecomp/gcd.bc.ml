let rec gcd a b =
  if a = b then a 
  else if a < b then gcd a (b - a)
  else gcd (a - b) b
in let () = gcd 10 18 
