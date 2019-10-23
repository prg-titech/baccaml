let rec swap arr i j =
  let x = arr.(i) in
  let y = arr.(j) in
  arr.(j) <- x;
  arr.(i) <- y;
  arr
in
let () =
  let arr = Array.make 10 0 in
  arr.(1) <- 42; arr.(2) <- 38;
  let arr = swap arr 1 2 in
  arr.(1)
