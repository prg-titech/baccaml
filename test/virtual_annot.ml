let print_array f arr = print_string "[|"; Array.iter (fun a -> f a; print_string "; ") arr; print_string "|] " in
let rec jit_dispatch _ _ _ _ = () in
let rec loop_start _ = () in
let rec loop_end _ = () in
let rec is_mj _ = false in

let rec loop stack old_base new_base ret n i =
  if n = i then (stack.(old_base + n) <- ret; old_base + n + 1)
  else (stack.(old_base + i) <- stack.(new_base + i);
        loop stack old_base new_base ret n (i + 1))
in

let rec frame_reset stack sp o l n =
  let ret = stack.(sp-n-l-1) in
  let old_base = sp - n - l - o - 1 in
  let new_base = sp - n in
  loop stack old_base new_base ret n 0
in

let rec interp stack sp bytecode pc =
  let instr = bytecode.(pc) in
  print_int pc; print_newline ();
  (* Printf.printf "is: %d\tsp: %d\tpc: %d\n" instr sp pc; *)
  (* print_newline (); print_array print_int stack; print_newline (); *)
  if instr = 0 then             (* ADD *)
    let v2 = stack.(sp - 1) in  (* sp: sp - 1 *)
    let v1 = stack.(sp - 2) in  (* sp: sp - 2 *)
    stack.(sp - 2) <- v1 + v2;  (* sp: sp - 1 *)
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 1 then        (* SUB *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 - v2;
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 3 then        (* LT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    let n = (if v1 < v2 then 1 else 0) in
    stack.(sp - 2) <- n;
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 4 then        (* CONST *)
    let c = bytecode.(pc + 1) in
    stack.(sp) <- c;
    interp stack (sp + 1) bytecode (pc + 2)
  else if instr = 5 then        (* JUMP_IF_ZERO *)
    let addr = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in
    if v = 0
    then interp stack (sp - 1) bytecode addr
    else interp stack (sp - 1) bytecode (pc + 2)
  else if instr = 6 then        (* CALL *)
    let addr = bytecode.(pc + 1) in
    if is_mj () then
      let r = interp stack sp bytecode addr in
      stack.(sp - 1) <- r;
      interp stack sp bytecode (pc + 2)
    else
      (stack.(sp) <- pc + 2;
       interp stack (sp + 1) bytecode addr)
  else if instr = 7 then        (* RET *)
    if is_mj () then
      stack.(sp - 1)
    else
     (let n = bytecode.(pc + 1) in
      let v = stack.(sp - 1) in   (* sp: sp - 1 *)
      let pc2 = stack.(sp - 2) in (* sp: sp - 2 *)
      print_int (pc2); print_newline ();
      stack.(sp - n - 2) <- v;    (* sp: sp - 2 - n + 1 = sp - 1 - n *)
      print_endline (string_of_int pc2);
      interp stack (sp - n - 1) bytecode pc2)
  else if instr = 8 then        (* DUP *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n - 1) in
    stack.(sp) <- v;
    interp stack (sp + 1) bytecode (pc + 2)
  else if instr = 9 then        (* HALT *)
    stack.(sp - 1)
  else if instr = 10 then       (* FRAME_RESET *)
    let o = bytecode.(pc + 1) in
    let l = bytecode.(pc + 2) in
    let n = bytecode.(pc + 3) in
    let sp2 = frame_reset stack sp o l n in
    interp stack sp2 bytecode (pc + 4)
  else if instr = 11 then       (* POP1 *)
    let v = stack.(sp - 1) in
    let _ = stack.(sp - 2) in
    stack.(sp - 2) <- v;
    interp stack (sp - 2) bytecode (pc + 1)
  else if instr = 12 then       (* LOOP_S *)
    (loop_start ();
     interp stack sp bytecode (pc + 1))
  else if instr = 13 then       (* LOOP_E *)
    (loop_end ();
     interp stack sp bytecode (pc + 1))
  else if instr = 14 then       (* JUMP *)
    let addr = bytecode.(pc + 1) in
    interp stack sp bytecode addr
  else
    -1000 in
let code = Array.make 100 0 in
let stack = Array.make 1000 0 in

(* fib *)
(* for meta tracing *)
(* 8 1 4 2 3 5 11 4 1 14 26 8 1 4 1 1 6 0 8 2 4 2 1 6 0 0 7 1 4 10 6 0 9 *)
code.(0) <- 8; code.(1) <- 1; code.(2) <- 4; code.(3) <- 2; code.(4) <- 3; code.(5) <- 5; code.(6) <- 11; code.(7) <- 8; code.(8) <- 1; code.(9) <- 14; code.(10) <- 26; code.(11) <- 8; code.(12) <- 1; code.(13) <- 4; code.(14) <- 1; code.(15) <- 1; code.(16) <- 6; code.(17) <- 0; code.(18) <- 8; code.(19) <- 2; code.(20) <- 4; code.(21) <- 2; code.(22) <- 1; code.(23) <- 6; code.(24) <- 0; code.(25) <- 0; code.(26) <- 7; code.(27) <- 1; code.(28) <- 4; code.(29) <- 5; code.(30) <- 6; code.(31) <- 0; code.(32) <- 9;
print_int ((interp stack 0 code 28))

(* for meta method *)
(* 8 0 4 2 3 5 11 4 1 14 26 8 0 4 1 1 6 0 8 1 4 2 1 6 0 0 7 4 10 6 0 9 *)
(* code.(0) <- 8; code.(1) <- 0; code.(2) <- 4; code.(3) <- 2; code.(4) <- 3; code.(5) <- 5; code.(6) <- 11; code.(7) <- 4; code.(8) <- 1; code.(9) <- 14; code.(10) <- 26; code.(11) <- 8; code.(12) <- 0; code.(13) <- 4; code.(14) <- 1; code.(15) <- 1; code.(16) <- 6; code.(17) <- 0; code.(18) <- 8; code.(19) <- 1; code.(20) <- 4; code.(21) <- 2; code.(22) <- 1; code.(23) <- 6; code.(24) <- 0; code.(25) <- 0; code.(26) <- 7; code.(27) <- 4; code.(28) <- 2; code.(29) <- 6; code.(30) <- 0; code.(31) <- 9;
print_int (interp stack 0 code 27) *)

(* meta tracing *)
(* code.(0) <- 8;
code.(1) <- 1;
code.(2) <- 4;
code.(3) <- 2;
code.(4) <- 3;
code.(5) <- 5;
code.(6) <- 11;
code.(7) <- 4;
code.(8) <- 1;
code.(9) <- 14;
code.(10) <- 21;
code.(11) <- 8;
code.(12) <- 1;
code.(13) <- 8;
code.(14) <- 2;
code.(15) <- 4;
code.(16) <- 1;
code.(17) <- 1;
code.(18) <- 6;
code.(19) <- 0;
code.(20) <- 0;
code.(21) <- 7;
code.(22) <- 1;
code.(23) <- 4;
code.(24) <- 10;
code.(25) <- 6;
code.(26) <- 0;
code.(27) <- 9;
print_int (interp stack 0 code 23) *)

(* tak *)
(* for meta tracing *)
(* code.(0) <- 8; code.(1) <- 3; code.(2) <- 8; code.(3) <- 3; code.(4) <- 3; code.(5) <- 5; code.(6) <- 11; code.(7) <- 8; code.(8) <- 2; code.(9) <- 14; code.(10) <- 61; code.(11) <- 8; code.(12) <- 3; code.(13) <- 4; code.(14) <- 1; code.(15) <- 1; code.(16) <- 8; code.(17) <- 3; code.(18) <- 8; code.(19) <- 3; code.(20) <- 6; code.(21) <- 0; code.(22) <- 8; code.(23) <- 3; code.(24) <- 4; code.(25) <- 1; code.(26) <- 1; code.(27) <- 8; code.(28) <- 3; code.(29) <- 8; code.(30) <- 6; code.(31) <- 6; code.(32) <- 0; code.(33) <- 8; code.(34) <- 3; code.(35) <- 4; code.(36) <- 1; code.(37) <- 1; code.(38) <- 8; code.(39) <- 6; code.(40) <- 8; code.(41) <- 6; code.(42) <- 6; code.(43) <- 0; code.(44) <- 8; code.(45) <- 2; code.(46) <- 8; code.(47) <- 2; code.(48) <- 8; code.(49) <- 2; code.(50) <- 10; code.(51) <- 3; code.(52) <- 3; code.(53) <- 3; code.(54) <- 4; code.(55) <- 0; code.(56) <- 5; code.(57) <- 0; code.(58) <- 11; code.(59) <- 11; code.(60) <- 11; code.(61) <- 7; code.(62) <- 3; code.(63) <- 4; code.(64) <- 12; code.(65) <- 4; code.(66) <- 6; code.(67) <- 4; code.(68) <- 0; code.(69) <- 6; code.(70) <- 0; code.(71) <- 9;
print_int (interp stack 5 code 62) *)

(* for meta method *)
(* 8 3 8 3 3 5 11 8 2 14 61 8 3 4 1 1 8 3 8 3 6 0 8 3 4 1 1 8 3 8 6 6 0 8 3 4 1 1 8 6 8 6 6 0 8 2 8 2 8 2 10 3 3 3 4 0 5 0 11 11 11 7 4 12 4 6 4 0 6 0 9 *)
(* code.(0) <- 8; code.(1) <- 3; code.(2) <- 8; code.(3) <- 3; code.(4) <- 3; code.(5) <- 5; code.(6) <- 11; code.(7) <- 8; code.(8) <- 2; code.(9) <- 14; code.(10) <- 61; code.(11) <- 8; code.(12) <- 3; code.(13) <- 4; code.(14) <- 1; code.(15) <- 1; code.(16) <- 8; code.(17) <- 3; code.(18) <- 8; code.(19) <- 3; code.(20) <- 6; code.(21) <- 0; code.(22) <- 8; code.(23) <- 3; code.(24) <- 4; code.(25) <- 1; code.(26) <- 1; code.(27) <- 8; code.(28) <- 3; code.(29) <- 8; code.(30) <- 6; code.(31) <- 6; code.(32) <- 0; code.(33) <- 8; code.(34) <- 3; code.(35) <- 4; code.(36) <- 1; code.(37) <- 1; code.(38) <- 8; code.(39) <- 6; code.(40) <- 8; code.(41) <- 6; code.(42) <- 6; code.(43) <- 0; code.(44) <- 8; code.(45) <- 2; code.(46) <- 8; code.(47) <- 2; code.(48) <- 8; code.(49) <- 2; code.(50) <- 10; code.(51) <- 3; code.(52) <- 3; code.(53) <- 3; code.(54) <- 4; code.(55) <- 0; code.(56) <- 5; code.(57) <- 0; code.(58) <- 11; code.(59) <- 11; code.(60) <- 11; code.(61) <- 7; code.(62) <- 4; code.(63) <- 12; code.(64) <- 4; code.(65) <- 6; code.(66) <- 4; code.(67) <- 0; code.(68) <- 6; code.(69) <- 0; code.(70) <- 9;
 * print_int (interp stack 6 code 62) *)

(* gcd *)
(* for meta tracing *)
(* code.(0) <- 8; code.(1) <- 2; code.(2) <- 4; code.(3) <- 1; code.(4) <- 3; code.(5) <- 5; code.(6) <- 11; code.(7) <- 8; code.(8) <- 1; code.(9) <- 14; code.(10) <- 44; code.(11) <- 8; code.(12) <- 2; code.(13) <- 8; code.(14) <- 2; code.(15) <- 3; code.(16) <- 5; code.(17) <- 32; code.(18) <- 8; code.(19) <- 1; code.(20) <- 8; code.(21) <- 3; code.(22) <- 1; code.(23) <- 8; code.(24) <- 3; code.(25) <- 8; code.(26) <- 1; code.(27) <- 6; code.(28) <- 0; code.(29) <- 11; code.(30) <- 14; code.(31) <- 44; code.(32) <- 8; code.(33) <- 2; code.(34) <- 8; code.(35) <- 2; code.(36) <- 1; code.(37) <- 8; code.(38) <- 2; code.(39) <- 8; code.(40) <- 1; code.(41) <- 6; code.(42) <- 0; code.(43) <- 11; code.(44) <- 7; code.(45) <- 2; code.(46) <- 4; code.(47) <- 10; code.(48) <- 4; code.(49) <- 18; code.(50) <- 6; code.(51) <- 0; code.(52) <- 9;
print_int (interp stack 0 code 46) *)

(* for meta method *)
(* code.(0) <- 8; code.(1) <- 1; code.(2) <- 4; code.(3) <- 1; code.(4) <- 3; code.(5) <- 5; code.(6) <- 11; code.(7) <- 8; code.(8) <- 0; code.(9) <- 14; code.(10) <- 44; code.(11) <- 8; code.(12) <- 1; code.(13) <- 8; code.(14) <- 1; code.(15) <- 3; code.(16) <- 5; code.(17) <- 32; code.(18) <- 8; code.(19) <- 0; code.(20) <- 8; code.(21) <- 2; code.(22) <- 1; code.(23) <- 8; code.(24) <- 2; code.(25) <- 8; code.(26) <- 1; code.(27) <- 6; code.(28) <- 0; code.(29) <- 11; code.(30) <- 14; code.(31) <- 44; code.(32) <- 8; code.(33) <- 1; code.(34) <- 8; code.(35) <- 1; code.(36) <- 1; code.(37) <- 8; code.(38) <- 1; code.(39) <- 8; code.(40) <- 1; code.(41) <- 6; code.(42) <- 0; code.(43) <- 11; code.(44) <- 7; code.(45) <- 4; code.(46) <- 21600; code.(47) <- 4; code.(48) <- 337500; code.(49) <- 6; code.(50) <- 0; code.(51) <- 9;
 * print_int (interp stack 0 code 45) *)
(* let s = get_micro_time () in
 * let rec loop_gcd n =
 *   if n = 0 then ()
 *   else let _ = interp stack 0 code 45 in loop_gcd (n - 1)
 * in
 * loop_gcd 10000000;
 * let e = get_micro_time () in
 * print_int (e - s) *)
