let%mj rec fib n =
  if n < 2 then n
  else fib(n-1) + fib(n-2)
in
let rec f x y =
  if x = 1 then y
  else
    let n = fib x in
    f (x-1) (n + y)
in let () = f 10 1
