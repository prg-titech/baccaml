let rec simple x y =
  let a = Array.make 10 0 in
  a.(5) <- x; a.(7) <- y;
  a.(5) + a.(7)
in let () = simple 10 20
