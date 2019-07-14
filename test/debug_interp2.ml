(* -*- mode: tuareg -*- *)
let jit_merge_point _ _ _ = () in
let can_enter_jit _ _ _ _ = () in
let is_mj _ = true in
let loop_start () = () in
let loop_end () = () in
let method_entry () = () in
let save_bp () = () in
let get_current_millis () = 0 in
let print_array f arr =
  print_string "[|";
  Array.iter (fun a -> f a; print_string "; ") arr;
  print_string "|] " in
let rec frame_reset stack old_base new_base ret n i =
  if n = i then (stack.(old_base + n) <- ret; old_base + n + 1)
  else (stack.(old_base + i) <- stack.(new_base + i);
        frame_reset stack old_base new_base ret n (i + 1))
in
let rec pop stack sp = stack.(sp - 1) in
let rec push stack sp v = stack.(sp) <- v in
let rec interp stack sp bytecode pc =
  jit_merge_point pc stack sp;
  let instr = bytecode.(pc) in
  print_string "pc: "; print_int pc; print_string " ";
  print_string "instr: "; print_int instr; print_string " ";
  print_string "sp: "; print_int sp; print_string " ";
  print_array print_int stack; print_newline ();
  if instr = 1 then             (* ADD *)
    let v2 = pop stack sp in  (* sp: sp - 1 *)
    let v1 = pop stack (sp - 1) in  (* sp: sp - 2 *)
    stack.(sp - 2) <- v1 + v2;  (* sp: sp - 1 *)
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 2 then        (* SUB *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 - v2;
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 3 then        (* LT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    let n = (if v1 < v2 then 1 else 0) in
    let _ = push stack (sp - 2) n in
    interp stack (sp - 1) bytecode (pc + 1)
  else if instr = 4 then        (* CONST *)
    let c = bytecode.(pc + 1) in
    stack.(sp) <- c;
    interp stack (sp + 1) bytecode (pc + 2)
  else if instr = 5 then        (* JUMP_IF_ZERO *)
    let addr = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in
    let sp2 = sp - 1 in
    if v = 0 then (
      if addr < pc then (
        can_enter_jit stack sp2 bytecode addr;
        interp stack (sp - 1) bytecode addr)
      else (
        interp stack (sp - 1) bytecode addr))
    else (
      interp stack (sp - 1) bytecode (pc + 2))
  else if instr = 6 then        (* CALL *)
    let addr = bytecode.(pc + 1) in
    if is_mj () then (
      stack.(sp) <- 100;
      let r = interp stack (sp + 1) bytecode addr in
      stack.(sp - 1) <- r;
      interp stack sp bytecode (pc + 2))
    else (
      stack.(sp) <- pc + 2;
      stack.(sp + 1) <- 200;
      interp stack (sp + 2) bytecode addr)
  else if instr = 7 then        (* RET *)
    let v = stack.(sp - 1) in    (* sp: sp - 1 *)
    let mode = stack.(sp - 2) in (* sp: sp - 2 *)
    if mode = 100 then v
    else (
      let n = bytecode.(pc + 1) in
      let pc2 = stack.(sp - 3) in (* sp: sp - 3 *)
      stack.(sp - n - 3) <- v;    (* sp: sp - 3 - n + 1 = sp - 2 - n *)
      interp stack (sp - n - 2) bytecode pc2)
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
    let ret = stack.(sp-n-l-1) in
    let old_base = sp - n - l - o - 1 in
    let new_base = sp - n in
    let sp2 = frame_reset stack old_base new_base ret n 0 in
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
    if addr < pc then (
      can_enter_jit stack sp bytecode addr;
      interp stack sp bytecode addr)
    else (
      interp stack sp bytecode addr)
  else if instr = 15 then
    (method_entry ();
     interp stack sp bytecode (pc + 1))
  else if instr = 16 then       (* DUP0 *)
    let v = stack.(sp - 1) in
    stack.(sp) <- v;
    interp stack (sp + 1) bytecode (pc + 1)
  else if instr = 17 then       (* POP0 *)
    let _ = stack.(sp - 1) <- 0 in
    interp stack (sp - 1) bytecode (pc + 1)
  else
    -1000 in

(* sum *)
let code = Array.make 50 0 in
code.(0) <- 8; code.(1) <- 1;
code.(2) <- 4; code.(3) <- 2;
code.(4) <- 3;
code.(5) <- 5; code.(6) <- 11;
code.(7) <- 4; code.(8) <- 1;
code.(9) <- 14; code.(10) <- 21;
code.(11) <- 8; code.(12) <- 1;
code.(13) <- 8; code.(14) <- 2;
code.(15) <- 4; code.(16) <- 1;
code.(17) <- 2;
code.(18) <- 6; code.(19) <- 0;
code.(20) <- 1;
code.(21) <- 7; code.(22) <- 1;
code.(23) <- 4; code.(24) <- 10;
code.(25) <- 6; code.(26) <- 0;
code.(27) <- 9;
let stk = Array.make 40 0 in
(* let res = interp stk 0 code 23 in print_int res; print_newline (); *)

(* fib *)
let code = Array.make 50 0 in
save_bp ();
code.(0) <- 15;
code.(1) <- 8; code.(2) <- 1;
code.(3) <- 4; code.(4) <- 2;
code.(5) <- 3;
code.(6) <- 5; code.(7) <- 12;
code.(8) <- 8; code.(9) <- 1;
code.(10) <- 14; code.(11) <- 27;
code.(12) <- 8; code.(13) <- 1;
code.(14) <- 4; code.(15) <- 1;
code.(16) <- 2;
code.(17) <- 6; code.(18) <- 1;
code.(19) <- 8; code.(20) <- 2;
code.(21) <- 4; code.(22) <- 2;
code.(23) <- 2;
code.(24) <- 6; code.(25) <- 1;
code.(26) <- 1;
code.(27) <- 7; code.(28) <- 1;
code.(29) <- 4; code.(30) <- 11;
code.(31) <- 6;
code.(32) <- 1;
code.(33) <- 9;
let st = Array.make 50 0 in
(* let res = (interp st 0 code 29) in print_int res; print_newline (); *)

(* fun call + loop *)
let code = Array.make 50 0 in
code.(0) <- 15;

code.(1) <- 8; code.(2) <- 1;
code.(3) <- 4; code.(4) <- 1;
code.(5) <- 3;
code.(6) <- 5; code.(7) <- 12;
code.(8) <- 4; code.(9) <- 1;
code.(10) <- 14; code.(11) <- 34;
code.(12) <- 8; code.(13) <- 1;
code.(14) <- 8; code.(15) <- 0;

(* loop *)
code.(16) <- 4; code.(17) <- 1;
code.(18) <- 2;
code.(19) <- 4; code.(20) <- 1;
code.(21) <- 8; code.(22) <- 1;
code.(23) <- 3;
code.(24) <- 5; code.(25) <- 28;
code.(26) <- 14; code.(27) <- 16;

code.(28) <- 17;
code.(29) <- 4; code.(30) <- 1;
code.(31) <- 2;
code.(32) <- 6; code.(33) <- 1;
code.(34) <- 7;

(* main *)
code.(36) <- 6; code.(37) <- 1;
code.(38) <- 9;

let stk = Array.make 50 0 in
stk.(0) <- 10;
let res = interp stk 1 code 36 in
print_int res; print_newline ();
()

  (* print_int res; print_newline () *)

(* loop *)
(* let code = Array.make 20 0 in
 * save_bp ();
 * code.(0) <- 15; code.(1) <- 2; code.(2) <- 7;
 * code.(3) <- 4; code.(4) <- 1;
 * code.(5) <- 2;
 * code.(6) <- 4; code.(7) <- 1;
 * code.(8) <- 8; code.(9) <- 1;
 * code.(10) <- 3;
 * code.(11) <- 5; code.(12) <- 15;
 * code.(13) <- 14; code.(14) <- 3;
 * code.(15) <- 9;
 * let st = Array.make 20 0 in
 * st.(0) <- 200000;
 * let s = get_current_millis () in
 * (\* let res = (interp st 1 code 3) in *\)
 * let e = get_current_millis () in
 * print_int (e - s); print_newline () *)
