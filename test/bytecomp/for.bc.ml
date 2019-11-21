let rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib(n-2)
in
let rec f n =
  let arr = Array.make 1 0 in
  for i = 0 to 10 do
    let x = fib i in
    let y = arr.(0) + x in
    arr.(0) <- y;
    ()
  done;
  arr.(0)
in let () = f 10
