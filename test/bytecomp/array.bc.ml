let rec simple x y =
  let a1 = Array.make 10 0 in
  let a2 = Array.make 10 a1 in
  a1.(5).(5) <- x; a2.(7).(7) <- y;
  a1.(5).(5) + a1.(7).(7)
in
let () = simple 10 20
