let rec mod_ n m =
  if n < m then n
  else mod_ (n - m) m
in
let rec prime_test cand i =
  let i2 = i * i in
  if cand < i2 then 1
  else if mod_ cand i = 0 then 0
  else prime_test cand (i + 1)
in
let () = (prime_test 31 2)
