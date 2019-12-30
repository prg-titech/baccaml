let rec simple x y =
  let arr = Array.make 10 0 in
  arr.(5) <- x; arr.(7) <- y;
  arr.(5) + arr.(7)
in
let () = simple 10 20
