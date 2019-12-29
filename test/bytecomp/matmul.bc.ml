let rec loop3 i k j a b c =
  if k < 0 then () else (
    let x = c.(i).(j) in
    let y = a.(i).(k) in
    let z = b.(k).(j) in
    let r = x + y * z  in
    c.(i).(j) <- r;
    loop3 i (k - 1) j a b c) in
let rec loop2 i m j a b c =
  if j < 0 then () else
  (loop3 i (m - 1) j a b c;
   loop2 i m (j - 1) a b c) in
let rec loop1 i m n a b c =
  if i < 0 then () else
  (loop2 i m (n - 1) a b c;
   loop1 (i - 1) m n a b c) in
let rec mul l m n a b c =
  loop1 (l - 1) m n a b c in
let rec init i n mat =
  if i < 0 then () else (
    let x = Array.make n 0 in
    mat.(i) <- x;
    init (i - 1) n mat)
in
let rec make m n =
  let dummy = Array.make n 1 in
  let mat = Array.make m dummy in
  init (m - 1) n mat;
  mat in
let () =
  let a = make 2 3 in
  let b = make 3 2 in
  let c = make 2 2 in
  a.(0).(0) <- 1; a.(0).(1) <- 2; a.(0).(2) <- 3;
  a.(1).(0) <- 4; a.(1).(1) <- 5; a.(1).(2) <- 6;
  b.(0).(0) <- 7; b.(0).(1) <- 8;
  b.(1).(0) <- 9; b.(1).(1) <- 10;
  b.(2).(0) <- 11; b.(2).(1) <- 12;
  let r = mul 2 3 2 a b c in
  r.(0).(0)
