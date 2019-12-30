let rec mod_ n m =
  if n < m then n
  else mod_ (n - m) m
in
let rec is_prime cand i =
  let i2 = i * i in
  if cand < i2 then 1
  else if mod_ cand i = 0 then 0
  else is_prime cand (i + 1)
in
let rec primes n acc =
  if n < 1 then acc
  else (
    let m = is_prime n 2 in
    primes (n-1) (acc+m))
in
let () = primes 121 0
