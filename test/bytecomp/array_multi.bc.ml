let rec simple x y =
  let dum = Array.make 10 0 in
  let mat = Array.make 10 dum in
  mat.(0).(0) <- x; mat.(0).(1) <- y;
  mat.(0).(0) + mat.(0).(1)
in
let () = simple 10 20
