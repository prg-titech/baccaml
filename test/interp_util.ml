let print_array f arr =
  print_string "[|";
  Array.iter (fun a -> f a; print_string "; ") arr;
  print_string "|] "

let jit_dispatch _ _ _ _ = ()

let loop_start _ = ()

let loop_end _ = ()

let is_mj _ = false
