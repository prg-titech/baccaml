let rec f n =
  let arr = Array.make 1 0 in
  for i = 0 to 10 do
    arr.(0) <- i;
    ()
  done;
  arr.(0)
in let () = f 10
