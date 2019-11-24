let%mj rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib(n-2)
in
let rec f n =
  let arr = Array.make 2 1 in
  for i = 1 to 21 do
    let x = fib i in
    let y = arr.(1) + x in
    arr.(1) <- y;
    ()
  done;
  arr.(1)
in let () = f 10
