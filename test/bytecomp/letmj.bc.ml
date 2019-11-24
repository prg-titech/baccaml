let%mj rec f x =
  if x < 2 then x
  else x + 1
in
let rec g y =
  f (y + 100)
in let () = g 10
