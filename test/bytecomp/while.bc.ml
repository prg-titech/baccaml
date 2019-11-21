let rec f n =
  let arr = Array.make 1 0 in
  while arr.(0) < 10 do
    let x = arr.(0) + 1 in
    arr.(0) <- x;
    ()
  done;
  arr.(0)
in let () = f 10
