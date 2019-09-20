open Syntax

module VM : sig
  (* instruction set: a stack machine *)
  type inst =
    | UNIT
    | ADD                       (* n2::n1::s -> (n1+n2)::s *)
    | SUB                       (* n2::n1::s -> (n1-n2)::s *)
    | MUL                       (* n2::n1::s -> (n1*n2)::s *)
    | LT                        (* n2::n1::s -> (n1<n2)::s *)
    | CONST (* n *)             (* s         -> n::s *)
    | JUMP_IF_ZERO (* addr *)   (* n::s      -> s *)
    | CALL (* fun-id *)         (* nm::...::n1::s -> pc::nm::...::n1::s *)
    | RET (* n *)               (* r::pc::v1...::vn::s -> r::s  *)
    | DUP (* n *)               (* v1::...::vn::s -> vn::v1::...::vn::s *)
    | HALT                      (* n -> . *)

    (* The next instruction assumes that the stack has (1) o values as
       the parameter to this frame, (2) the return address from this
       frame, (3) l values as the local variables in this frame, and
       (4) n values as the new parameter, and deletes (1) and (3),
       moves (2) to the top of the stack.

       before:
       (stack top) [n new args][l local vars][ret][o old args]...(bottom)
       after:
                                 (stack top) [ret][n new args]...(bottom)
    *)

    | FRAME_RESET (* o l n *)
    | POP1                      (* n2::n1::s ->  n2::s *)
    | JUMP (* addr *)
    | METHOD_ENTRY
    (* the following constructors do not represent instructions but
       are defined for expressing operands of some instructions as
       well as label declarations and references *)
    | Literal of int
    | Lref of string
    | Ldef of string

  val string_of : inst -> string (* for debugging *)

  val show_inst : inst -> string (* for debugging *)

  type fundef_bin_t = int array
  type fundef_asm_t = inst array
  val run_bin : fundef_bin_t -> int
  val run_asm : fundef_asm_t -> int
end = struct
  type inst =
    | UNIT
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
    | JUMP
    | METHOD_ENTRY
    | Literal of int
    | Lref of string
    | Ldef of string
  [@@deriving show]

  let max_stack_depth = 100000

  (* the next array determines the opcode *)
  let insts = [|
    UNIT;
    ADD;
    MUL;
    SUB;
    LT;
    CONST;
    JUMP_IF_ZERO; (* LOAD; STORE; *)
    CALL;
    RET;
    DUP;
    HALT;
    FRAME_RESET;
    POP1;
    JUMP;
    METHOD_ENTRY;
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
                  (code_at_pc code pc) (dump_stack stack); *)

    if pc<0 then fst(pop stack) else
      let i,pc = fetch code pc in
      match insts.(i) with
      | UNIT ->
        interp code (pc + 1) stack
      | ADD ->
        let v2,stack = pop stack in
        let v1,stack = pop stack in
        let    stack = push stack (v1+v2) in
        interp  code pc stack
      | SUB -> let v2,stack = pop stack in
        let v1,stack = pop stack in
        let    stack = push stack (v1-v2) in
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
      | JUMP (* addr *)->
        let n,_ = fetch code pc in
        interp code n stack
      | METHOD_ENTRY ->
        interp code pc stack

  (* run the given program by calling the function id 0 *)
  type fundef_bin_t = int array
  let run_bin : fundef_bin_t -> int = fun fundefs ->
    let stack = (push (make_stack ()) (-987)) in
    interp fundefs 0 stack

  (* convert the given program into binary, and then run *)
  type fundef_asm_t = inst array
  let run_asm : fundef_asm_t -> int = fun fundefs ->
    run_bin (Array.map int_of_inst fundefs)

  (* testing *)
  let test_ex arity pc name instrs ostack lvars expected =
    let code = Array.of_list (arity::(List.map int_of_inst instrs)) in
    let sp = List.length ostack in
    let ostack = Array.init max_stack_depth (fun i ->
        if i < sp then List.nth ostack i else 0) in
    let v = interp code pc (sp, ostack) in
    if v=expected
    then (Printf.printf "OK: %s\n" name)
    else failwith
        (Printf.sprintf "NG: %s expected=%d actual=%d\n" name expected v )
  let test = test_ex 0 1

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
      [HALT; CALL; Literal 1;] [] [] 4
  let _ = test_ex 0 1 "RET simple"
      [RET; Literal 1; ADD; HALT] [4;5;3;6] [] 10
  let _ = test_ex 2 8 "CALL and RET"
      (* stack =     [8;5;4]  --- 8 is the return addr. *)
      [DUP; Literal 2;    (* stack =   [4;8;5;4] *)
       DUP; Literal 2;    (* stack = [5;4;8;5;4] *)
       ADD;               (* stack =   [9;8;5;4] *)
       RET; Literal 2;    (* stack =         [9] *)
       (* stack =       [5;4] *)
       CALL; Literal 1;   (* stack =         [9] *)
       HALT]
      [ 4;5 ] [] 9
  let _ = test_ex 2 8 "CALL mul add"
      [DUP; Literal 2; (* 1;2 *)
       DUP; Literal 2; (* 3;4 *)
       MUL;             (* 5 *)
       RET; Literal 2;  (* 6;7 *)
       CALL; Literal 1;  (* 8;9 *)
       ADD;              (* 10 *)
       HALT]              (* 11 *)
      [ 1; 2; 3 ] [] 7
end

module Compiler = struct

  (* generate a unique label id *)
  let gen_label, reset =
    let counter = ref 0 in
    ((fun () -> let l = !counter in
       counter := (!counter + 1);
       ("$"^(string_of_int l))),
     fun () -> counter := 0)

  (* compilation environment maps local variable names to local
     variable numbers *)
  let lookup env var =
    fst(List.find (fun (_,v) -> var=v)
          (List.mapi (fun idx v -> (idx,v)) env))
  let extend_env env var = var :: env
  let shift_env env = extend_env env "*dummy*"
  let return_address_marker = "$ret_addr"
  let build_arg_env args = return_address_marker::(List.rev args)
  (* computes the number of arguments to this frame.  The stack has a
     shape like [...local vars...][ret addr][..args...], the return
     address position from the top indicates the number of local
     variables on top of the return address. *)
  let arity_of_env env =
    let num_local_vars = lookup env return_address_marker in
    (List.length env - num_local_vars  - 1, num_local_vars)

  (* compilation of expressions *)
  let rec compile_exp fenv exp env =
    VM.(match exp with
        | Int n -> [CONST; Literal n]
        | Var v -> [DUP; Literal(lookup env v)]
        | Add(e1,e2) -> (compile_exp fenv e1 env) @
                        (compile_exp fenv e2 (shift_env env)) @ [ADD]
        | Sub(e1, e2) -> (compile_exp fenv e1 env) @
                         (compile_exp fenv e2 (shift_env env)) @ [SUB]
        | Mul(e1,e2) -> (compile_exp fenv e1 env) @
                        (compile_exp fenv e2 (shift_env env)) @ [MUL]
        | LT(e1,e2) -> (compile_exp fenv e1 env) @
                       (compile_exp fenv e2 (shift_env env)) @ [LT]
        | If(cond,then_exp,else_exp) ->
          let l2,l1 = gen_label(),gen_label() in
          (compile_exp fenv cond env)
          @ [JUMP_IF_ZERO; Lref l1]
          @ (compile_exp fenv then_exp env)
          @ [JUMP; Lref l2; (* unconditional jump *)
             Ldef l1]
          @ (compile_exp fenv else_exp env)
          @ [Ldef l2]
        | Call(fname, rands) ->
          (List.flatten
             (List.rev
                (fst
                   (List.fold_left (fun (rev_code_list,env) exp ->
                        (compile_exp fenv exp env)::rev_code_list,
                        shift_env env)
                       ([], env) rands))))
          @ [CALL; Lref fname] (* call using a label *)
        (* self tail calls are compiled with FRAME_RESET which moves
           the computed arguments to the position of the actual
           parameters in the current frame. *)
        | TCall(fname, rands) ->
          let old_arity,local_size = arity_of_env env in
          let new_arity = List.length rands in
          (List.flatten
             (List.rev
                (fst
                   (List.fold_left (fun (rev_code_list,env) exp ->
                        (compile_exp fenv exp env)::rev_code_list,
                        shift_env env)
                       ([], env) rands))))
          @ [FRAME_RESET;
             Literal old_arity; Literal local_size; Literal new_arity;
             JUMP; Lref fname]
        | Let(var,exp,body) ->
          let ex_env = extend_env env var in
          (compile_exp fenv exp env)            (* in old env *)
          @ (compile_exp fenv body ex_env)      (* in extended env *)
          @ [POP1]                              (* drop the value *)
      )

  let rec tail_elim fname = function
    | If(cond,then_exp,else_exp) ->
      If(cond, tail_elim fname then_exp, tail_elim fname else_exp)
    | Call(fname', rands) ->
      if fname=fname'
      then TCall(fname', rands)
      else Call(fname', rands)
    | Let(var,exp,body) ->
      Let(var,exp, tail_elim fname body)
    | others -> others

  (* resolving labels *)
  let assoc_if subst elm =
    try List.assoc elm subst with
    | Not_found  -> elm

  (* [...;Ldef a;...] -> [...;a,i;...] where i is the index of the
     next instruction of Ldef a in the list all Ldefs are removed
     e.g., [_;Ldef 8;_;Ldef 7;_] ==> [8,1; 7,2]
  *)
  let make_label_env instrs =
    snd(List.fold_left
          VM.(fun (addr,env) -> function
              | Ldef n -> (addr, (Lref n, Literal(addr))::env)
              | _ ->      (addr+1, env)) (0,[]) instrs)

  (* remove all Ldefs and replace Lrefs with Literals *)
  let resolve_labels instrs =
    List.filter (function VM.Ldef _ -> false | _ -> true)
      (List.map (assoc_if (make_label_env instrs)) instrs)

  let make_fenv exp =
    fun name ->
    find_fundefs exp
    |> List.mapi (fun i fundef -> (i, fundef))
    |> List.find (fun (_, {name=n}) -> name=n)
    |> fst

  let compile_fun_body fenv name arity exp env =
    VM.METHOD_ENTRY ::
    (VM.Ldef name)::
    (compile_exp fenv exp env) @ (
      if name = "main" then [VM.HALT]
      else [VM.RET; VM.Literal arity])


  let compile_fun fenv {name; args; body} =
    compile_fun_body fenv name (List.length args)
      (tail_elim name body)
      (build_arg_env args)

  let compile_funs fundefs =
    let fenv name = fst(List.find (fun (_,{name=n}) -> name=n)
                          (List.mapi (fun idx fdef -> (idx,fdef))
                             fundefs)) in
    Array.of_list(resolve_labels
                    (List.flatten
                       (List.map (compile_fun fenv) fundefs)))

  let compile_from_exp (exp : Syntax.exp) : VM.inst array =
    let fundefs = find_fundefs exp in
    let main = fundefs |> List.find (fun { name } -> name = "main") in
    let others = fundefs |> List.filter (fun { name } -> name <> "main") in
    (compile_funs (main :: others))


  (* for testing *)
  module Test = struct

    let string_of_code_list clist =
      String.concat ";" (List.map VM.string_of clist)
    let print_insts insts =
      let rec print_inst inst =
        match inst with
        | VM.Literal i -> print_int i; print_newline ()
        | _ -> VM.string_of inst |> print_string; print_string " "
      in
      insts |> List.map print_inst |> ignore
    let print_code_pair code1 code2 =
      Printf.sprintf "code1: %s\ncode2: %s\n"
        (string_of_code_list code1) (string_of_code_list code2)
    let test_ex compiler name exp expected =
      let actual = compiler (fun _ -> 47) exp
          (extend_env (build_arg_env ["x";"y"]) "w") in
      if actual = expected
      then (Printf.printf "%s: %s\n" name "OK")
      else failwith (Printf.sprintf "%s: NG\n%s"
                       name (print_code_pair expected actual))
    let test = test_ex compile_exp
  end
end
