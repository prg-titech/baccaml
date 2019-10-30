open OUnit2
open Jit
open Jit_prof

module Make_prof_test = Make_prof(struct let threshold = 10;; let typ = `Meta_tracing end)

let test_prof _ =
  Make_prof_test.register (1, "tracetj0");
  for i = 1 to 20 do
    Make_prof_test.count_up 1;
  done;
  assert_equal (Make_prof_test.over_threshold 1) true

let suite =
  "TestJitProf" >:::
    ["test_prof" >:: test_prof]

let _ =
  run_test_tt_main suite
