open Syntax

type inst =
  | ADD
  | SUB
  | MUL
  | LT
  | CONST
  | JUMP_IF_ZERO (* addr *)
  | CALL (* fun-id *)
  | RET
  | DUP (* n *)
  | HALT
  | FRAME_RESET (* o l n *)
  | POP1
  | LOOP_S
  | Literal of int
  | Lref of string
  | Ldef of string
[@@deriving show]

let log_flg = ref false
let log s = if !log_flg then print_endline ("[INFO] " ^ s)

let max_stack_depth = 1024

(* the next array determines the opcode *)
let insts =
  [| ADD;
     SUB;
     MUL;
     LT;
     CONST;
     JUMP_IF_ZERO;
     CALL;
     RET;
     DUP;
     HALT;
     FRAME_RESET;
     POP1;
     LOOP_S
  |]

let index_of element array =
  fst(List.find (fun (_,v) -> v=element)
        (List.mapi (fun idx v -> (idx,v)) (Array.to_list array)) )
let int_of_inst = function
  | Literal n -> n
  | Ldef lbl | Lref lbl -> failwith("unresolved "^lbl)
  | inst -> index_of inst insts
let string_of = function
  | Literal n -> Printf.sprintf "Literal %d" n
  | Ldef n    -> Printf.sprintf "Ldef %s" n
  | Lref n    -> Printf.sprintf "Lref %s" n
  | i         -> string_of_int(int_of_inst i)

(* operand stack
   We pair the stack pointer and the array of values.  Though the
   reference to the arry is not changed over the push/pop
   operations, those operations return a new pair just for
   convenience.  If we switch to the MinCaml implementation, we
   should decouple the pair.
*)

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
    | ADD ->
      log "ADD";
      let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (v1+v2) in
      interp  code pc stack
    | SUB ->
      log "SUB";
      let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (v1-v2) in
      interp  code pc stack
    | MUL ->
      log "MUL";
      let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (v1*v2) in
      interp  code pc stack
    | LT ->
      log "LT";
      let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (if v1<v2 then 1 else 0) in
      interp  code pc stack
    | CONST -> let c,pc = fetch code pc in
      log "CONST";
      let stack = push stack c in
      interp  code pc stack
    | JUMP_IF_ZERO (* addr *) ->
      log "JUMP_IF_ZERO";
      let addr,pc = fetch code pc in
      let v,stack = pop stack in
      (* interp  code (if v=0 then addr else pc) stack *)
      if v=0
      then interp code addr stack
      else interp code pc   stack
    | CALL (* addr *) ->
      log "CALL";
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
      log "RET";
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
      log @@ "DUP " ^ (string_of_int n);
      interp  code pc stack
    | HALT -> log "HALT"; fst(pop stack)    (* just return the top value *)
    | FRAME_RESET (* n *) ->
      let o,pc = fetch code pc in
      let l,pc = fetch code pc in
      let n,pc = fetch code pc in
      let stack = frame_reset stack o l n in
      interp  code pc stack
    | POP1 ->
      log "POP1";
      let v,stack = pop stack in
      let _,stack = pop stack in
      let stack = push stack v in
      interp  code pc stack
    | LOOP_S ->
      log "LOOP_S";
      interp code (pc + 1) stack
    | Literal i ->
      failwith @@ Printf.sprintf "unresolved. Literal %d" i
    | Ldef s | Lref s ->
      failwith @@ Printf.sprintf "unresolved. Lref/Ldef %s." s

(* run the given program by calling the function id 0 *)
type fundef_bin_t = int array
let run_bin : fundef_bin_t -> int = fun fundefs ->
  let stack = (push (make_stack ()) (-987)) in
  interp fundefs 0 stack

(* convert the given program into binary, and then run *)
type fundef_asm_t = inst array
let run_asm : fundef_asm_t -> int = fun fundefs ->
  run_bin (Array.map int_of_inst fundefs)
