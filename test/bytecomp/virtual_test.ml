open Bytegen_lib

open OUnit2

module VM_test = struct
  open VM
  open Value

  (* testing *)
  let test_ex : int -> int -> string -> inst list -> int list -> int list -> int -> unit =
    fun arity pc name instrs ostack lvars expected ->
      let code = Array.of_list (arity::(List.map int_of_inst instrs)) in
      let sp = List.length ostack in
      let ostack = Array.init max_stack_depth (fun i ->
                       (if i < sp then List.nth ostack i else 0)
                       |> value_of_int) in
      let v = interp code pc (sp, ostack) in
      if int_of_value v=expected
      then (Printf.printf "OK: %s\n" name)
      else failwith
          (Printf.sprintf "NG: %s expected=%d actual=%d\n" name expected (int_of_value v) )

  let test : string -> inst list -> int list -> int list -> int -> unit =
    test_ex 0 1

  let _ = test "ADD" [ADD; HALT] [ 1; 2 ] [] 3
  let _ = test "MUL" [MUL; HALT] [ 2; 3 ] [] 6
  let _ = test "LT true" [LT; HALT] [ 2; 3 ] [] 1
  let _ = test "LT false" [LT; HALT] [ 3; 2 ] [] 0
  let _ = test "CONST" [CONST; Literal 3; HALT] [ ] [] 3
  let _ = test "JUMP_IF_ZERO taken"
      [JUMP_IF_ZERO; Literal 6; (* 1;2 *)
       CONST; Literal 1;        (* 3;4 *)
       HALT;                     (* 5 *)
       CONST; Literal 2;        (* 6;7 *)
       HALT] [ 0 ] [] 2
  let _ = test "JUMP_IF_ZERO not taken"
      [JUMP_IF_ZERO; Literal 6; (* 1;2 *)
       CONST; Literal 1;        (* 3;4 *)
       HALT;                     (* 5 *)
       CONST; Literal 2;        (* 6;7 *)
       HALT] [ 8 ] [] 1
  (* let _ = test "LOAD" [LOAD; Literal 0; HALT] [] [ 1 ] 1
   * let _ = test "STORE" [STORE; Literal 0;
   *                       LOAD; Literal 0; HALT] [3] [ ] 3 *)
  let _ = test "DUP"            (* TOP   3 2 BOTTOM *)
      [DUP; Literal 1;    (* TOP 2 3 2 BOTTOM *)
       LT;                (* TOP   0 2 BOTTOM *)
       ADD;               (* TOP     2 BOTTOM *)
       HALT] [ 2;3 ] [] 2
  let  _ = test "FRAME_RESET"            (* TOP 6 5 4 3 2 1 BOTTOM *)
      [FRAME_RESET; Literal 2; Literal 1; Literal 1;
       (* TOP       4 6 1 BOTTOM *)
       MUL;                       (* TOP        24 1 BOTTOM *)
       ADD;                       (* TOP          25 BOTTOM *)
       HALT] [ 1;2;3;4;5;6 ] [] 25
  let _ = test "POP1"
      [POP1; ADD; HALT] [ 1;2;3 ] [] 4

  let _ = test_ex 0 2 "CALL simple"
      [HALT; CALL; Literal 1; Literal 1;] [] [] 5
  let _ = test_ex 0 1 "RET simple"
      [RET; Literal 1; ADD; HALT] [4;5;3;6] [] 10
  let _ = test_ex 2 8 "CALL and RET"
      (* stack =     [8;5;4]  --- 8 is the return addr. *)
      [DUP; Literal 2;    (* stack =   [4;8;5;4] *)
       DUP; Literal 2;    (* stack = [5;4;8;5;4] *)
       ADD;               (* stack =   [9;8;5;4] *)
       RET; Literal 2;    (* stack =         [9] *)
       (* stack =       [5;4] *)
       CALL; Literal 1; Literal 2;  (* stack =         [9] *)

       HALT]
      [ 4;5 ] [] 9
  let _ = test_ex 2 8 "CALL mul add"
      [DUP; Literal 2;  (* 1;2 *)
       DUP; Literal 2;  (* 3;4 *)
       MUL;             (* 5 *)
       RET; Literal 2;  (* 6;7 *)
       CALL; Literal 1;  Literal 2; (* 8;9 *)
       ADD;             (* 10 *)
       HALT]            (* 11 *)
      [ 1; 2; 3 ] [] 7

 end
