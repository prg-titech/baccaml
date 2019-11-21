open OUnit2

open Bytegen_lib
open VM

let test_print _ =
  let insts = [CONST; Literal 1; CONST; Literal 2; ADD; HALT] in
  Util.print_code insts

let suite =
  "UtilTest" >::: [
      "test_print" >:: test_print;
    ]

let _ =
  run_test_tt_main suite
