let () =
  let arr = Array.make 10 0 in
  arr.(5) <- 1; arr.(7) <- 2;
  arr.(5) + arr.(7)
