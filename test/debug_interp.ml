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
let rec interp stack sp cstack cp bytecode pc =
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
    interp stack (sp - 1) cstack cp bytecode (pc + 1)
  else if instr = 2 then        (* SUB *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    stack.(sp - 2) <- v1 - v2;
    interp stack (sp - 1) cstack cp bytecode (pc + 1)
  else if instr = 3 then        (* LT *)
    let v2 = stack.(sp - 1) in
    let v1 = stack.(sp - 2) in
    let n = (if v1 < v2 then 1 else 0) in
    let _ = push stack (sp - 2) n in
    interp stack (sp - 1) cstack cp bytecode (pc + 1)
  else if instr = 4 then        (* CONST *)
    let c = bytecode.(pc + 1) in
    stack.(sp) <- c;
    interp stack (sp + 1) cstack cp bytecode (pc + 2)
  else if instr = 5 then        (* JUMP_IF_ZERO *)
    let addr = bytecode.(pc + 1) in
    let v = stack.(sp - 1) in
    let sp2 = sp - 1 in
    if v = 0 then (
      if addr < pc then (
        can_enter_jit stack sp2 bytecode addr;
        interp stack (sp - 1) cstack cp bytecode addr)
      else (
        interp stack (sp - 1) cstack cp bytecode addr))
    else (
      interp stack (sp - 1) cstack cp bytecode (pc + 2))
  else if instr = 6 then        (* CALL *)
    let addr = bytecode.(pc + 1) in
    if is_mj () then (
      cstack.(cp) <- true;
      let r = interp stack sp cstack (cp + 1) bytecode addr in
      stack.(sp - 1) <- r;
      interp stack sp cstack cp bytecode (pc + 2))
    else (
      cstack.(cp) <- false;
      stack.(sp) <- pc + 2;
      interp stack (sp + 1) cstack (cp + 1) bytecode addr)
  else if instr = 7 then        (* RET *)
    let v = stack.(sp - 1) in
    let mode = cstack.(cp - 1) in
    if mode then v
    else (
      let n = bytecode.(pc + 1) in
      let v = stack.(sp - 1) in   (* sp: sp - 1 *)
      let pc2 = stack.(sp - 2) in (* sp: sp - 2 *)
      stack.(sp - n - 2) <- v;    (* sp: sp - 2 - n + 1 = sp - 1 - n *)
      interp stack (sp - n - 1) cstack cp bytecode pc2)
  else if instr = 8 then        (* DUP *)
    let n = bytecode.(pc + 1) in
    let v = stack.(sp - n - 1) in
    stack.(sp) <- v;
    interp stack (sp + 1) cstack cp bytecode (pc + 2)
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
    interp stack sp2 cstack cp bytecode (pc + 4)
  else if instr = 11 then       (* POP1 *)
    let v = stack.(sp - 1) in
    let _ = stack.(sp - 2) in
    stack.(sp - 2) <- v;
    interp stack (sp - 2) cstack cp bytecode (pc + 1)
  else if instr = 12 then       (* LOOP_S *)
    (loop_start ();
     interp stack sp cstack cp bytecode (pc + 1))
  else if instr = 13 then       (* LOOP_E *)
    (loop_end ();
     interp stack sp cstack cp bytecode (pc + 1))
  else if instr = 14 then       (* JUMP *)
    let addr = bytecode.(pc + 1) in
    if addr < pc then (
      can_enter_jit stack sp bytecode addr;
      interp stack sp cstack cp bytecode addr)
    else (
      interp stack sp cstack cp bytecode addr)
  else if instr = 15 then
    (method_entry ();
     interp stack sp cstack cp bytecode (pc + 1))
  else if instr = 16 then       (* DUP0 *)
    let v = stack.(sp - 1) in
    stack.(sp) <- v;
    interp stack (sp + 1) cstack cp bytecode (pc + 1)
  else
    -1000 in

(* debug *)
let code = Array.make 20 0 in
code.(0) <- 15;
code.(1) <- 8; code.(2) <- 1;
code.(3) <- 4; code.(4) <- 1;
code.(5) <- 2;
code.(6) <- 7;
code.(7) <- 6; code.(8) <- 0;
code.(9) <- 9;
let stk = Array.make 10 0 in
stk.(0) <- 10;
(* print_int (interp stk 1 code 7); print_newline (); *)

(* fib *)
let code = Array.make 50 0 in
save_bp ();
code.(0) <- 15;
code.(1) <- 16;
code.(2) <- 4;
code.(3) <- 2;
code.(4) <- 3;
code.(5) <- 5;
code.(6) <- 10;
code.(7) <- 16;
code.(8) <- 14;
code.(9) <- 24;
code.(10) <- 16;
code.(11) <- 4;
code.(12) <- 1;
code.(13) <- 2;
code.(14) <- 6;
code.(15) <- 1;
code.(16) <- 8;
code.(17) <- 1;
code.(18) <- 4;
code.(19) <- 2;
code.(20) <- 2;
code.(21) <- 6;
code.(22) <- 1;
code.(23) <- 1;
code.(24) <- 7;
code.(25) <- 6;
code.(26) <- 1;
code.(27) <- 9;
let st = Array.make 30 (0) in
st.(0) <- 10;
let cst = Array.make 30 false in
let s = get_current_millis () in
let res = (interp st 1 cst 0 code 25) in
let e = get_current_millis () in
print_int (e - s); print_newline ();
print_int res; print_newline ();

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
