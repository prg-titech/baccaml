let print_array f arr =
  print_string "[|";
  Array.iter
    (fun a -> f a; print_string "; ")
    arr;
  print_string "|] " in

  type stack = int * int array
  let push : stack -> int -> stack =
    fun (sp,stack) v -> stack.(sp)<-v; (sp+1,stack)
  let pop  : stack -> (int * stack) =
    fun (sp,stack) -> (stack.(sp-1), (sp-1, stack))
  let take : stack -> int -> int =
    fun (sp,stack) n -> stack.(sp-n-1)
  let drop : stack -> int -> stack =
    fun (sp,stack) n -> (sp-n,stack)
  let frame_reset : stack -> int -> int -> int -> stack =
    fun (sp,stack) o l n ->
      let ret = stack.(sp-n-l-1) in (* save return address *)
      let old_base = sp - n - l - o - 1 in
      let new_base = sp - n in
      let rec loop i =
        if n=i then (stack.(old_base+n)<-ret; (old_base+n+1, stack))
        else (stack.(old_base+i)<-stack.(new_base+i);
              loop (i+1)) in
      loop 0
  let make_stack () = (0, Array.make max_stack_depth 0)

  (* let test_stack =(9,  [|1;2;3;4;5;6;7;8;9|])
   * let reset_result = frame_reset test_stack 2 2 3
   * let _ = Printf.printf "reset -> [%s]\n"
   *           (String.concat ";"
   *              (List.map string_of_int (Array.to_list (snd reset_result))))
   * let _ = assert  ((5, [|1;7;8;9;4;6;7;8;9|]) = reset_result) *)

  (* fetch one integer from the code, and advance the program counter *)
  let fetch code pc = (code.(pc), pc+1)

  let code_at_pc code pc =
    if 0<=pc && pc<Array.length(code) then
      Printf.sprintf "code[%d..]=%d %d" pc code.(pc)
        (if pc+1<Array.length(code) then code.(pc+1) else -1)
    else Printf.sprintf "pc=%d" pc

  let dump_stack (sp,stack) =
    let rec loop i = if i=sp then ""
      else (string_of_int stack.(i))^";"^(loop (i+1)) in
    "["^(loop 0)^"]"

  (* when the VM won't stop, you may turn on the following function to
     forcingly terminate after executing a certain amount of
     instructions *)
  let checkpoint =
    if false
    then let counter = ref 5000 in
      fun () ->
        if !counter = 0
        then failwith "expired!"
        else counter := !counter - 1
    else fun () -> ()

let rec interp  code pc stack =
  checkpoint ();
  (* Printf.printf "%s %s\n"
   *               (code_at_pc code pc) (dump_stack stack); *)

  if pc<0 then fst(pop stack) else
    let i,pc = fetch code pc in
    match insts.(i) with
    | ADD -> let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (v1+v2) in
      interp  code pc stack
    | MUL -> let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (v1*v2) in
      interp  code pc stack
    | LT ->  let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (if v1<v2 then 1 else 0) in
      interp  code pc stack
    | CONST -> let c,pc = fetch code pc in
      let stack = push stack c in
      interp  code pc stack
    | JUMP_IF_ZERO (* addr *) ->
      let addr,pc = fetch code pc in
      let v,stack = pop stack in
      (* interp  code (if v=0 then addr else pc) stack *)
      if v=0
      then interp code addr stack
      else interp code pc   stack
    | CALL (* addr *) ->
      (* calling a function will create a new operand stack and lvars  *)
      let addr,pc = fetch code pc  in
      let stack = push stack pc in (* save return address *)
      (* (let (sp,s)=stack in
       *  if 2<sp then
       *    (Printf.printf "%d CALL %d [%d %d ...]\n" (pc-2) addr
       *       (s.(sp-2)) (s.(sp-3)))
       *  else ())
       * ; *)
      interp  code addr stack
    | RET (* n *) ->
      (* let pc0 = pc-1 in *)
      let n,pc = fetch code pc in
      let v,stack = pop stack in (* return value *)
      let pc,stack = pop stack in (* return address *)
      let stack = drop stack n in (* delete arguments *)
      let stack = push stack v in (* restore return value *)
      (* Printf.printf "%d RET with %d to %d\n" pc0 v pc; *)
      interp  code pc stack
    | DUP ->
      let n,pc = fetch code pc in
      let stack = push stack (take stack n) in
      interp  code pc stack
    | HALT -> fst(pop stack)    (* just return the top value *)
    | FRAME_RESET (* n *) ->
      let o,pc = fetch code pc in
      let l,pc = fetch code pc in
      let n,pc = fetch code pc in
      let stack = frame_reset stack o l n in
      interp  code pc stack
    | POP1 ->
      let v,stack = pop stack in
      let _,stack = pop stack in
      let stack = push stack v in
      interp  code pc stack

(*
   CONST 1
   LT
   JUMP_IF_ZERO 8
   CONST 1
   RET
   CONST 1
   SUB
   CALL 0
   DUP
   CONST 2
   SUB
   CALL 0
   DUP
   ADD
   RET
 *)

let bytecode = [|
  4; 1;
  2;
  5; 8;
  4; 1;
  7;
  8; 1;
  4; 1;
  1;
  6; 0;
  8; 2;
  4; 2;
  1;
  6; 0;
  0;
  7;
|] in

let stack = Array.make 20 0 in
stack.(0) <- 5;
print_int (interp bytecode 0 stack 1)
