let rec interp a b c d =
  if a = 0 then
    let r =
      can_enter_jit a b c (fun _ -> interp a b c 1)
     (* if over_threshold then jump to a tracer else exec cont.  *)
    in
    r
  else
    1
in print_int (interp 1 2 3 4)
