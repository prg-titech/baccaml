;;
let rec f a b = a + b in
let rec g d =
  let e = d + 1 in
  let e2 = f 1 2 in
  e + e2
in
let res = g 2 in
print_int res
