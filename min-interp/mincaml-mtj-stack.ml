(* simple compiler and bytecode interpreter -- stack machine version *)

(* abstract syntax tree for a simple functional language *)
type var = string
type exp =
  | Int of int
  | Var of var
  | Add of exp * exp 
  | Mul of exp * exp
  | LT of exp * exp             (* less than *)
  | If of exp * exp * exp
  | Call of var * exp list
  | Let of var * exp * exp
  | TCall of var * exp list     (* tail call --- internal only *)
type fundef = {name:var; args:var list; body: exp}

(* sample programs *)
let fact = {
  name= "fact"; args= ["n"];
  body= If(LT(Var "n", Int 1),
           Int 1,
           Mul(Var "n", Call("fact",[Add(Var "n", Int(-1))])))
}
let fib = {
  name= "fib"; args= ["n"];
  body= If(LT(Var "n", Int 2),
           Var "n",
           Add(Call("fib",[Add(Var "n", Int(-1))]),
               Call("fib",[Add(Var "n", Int(-2))])))
}

(* simple interpreter as a reference implementation *)
(* variable environment : var -> value *)
let null_env name = raise Not_found
let extend_env env var value name =
  if var = name then value else env name
let make_env vars vals =
  List.fold_left2 extend_env null_env vars vals

(* the core interpreter *)
let rec interp fundefs exp env = match exp with
  | Int n -> n
  | Var v -> env v
  | Add(e1,e2) -> (interp fundefs e1 env) + (interp fundefs e2 env)
  | Mul(e1,e2) -> (interp fundefs e1 env) * (interp fundefs e2 env)
  | LT(e1,e2)  -> if (interp fundefs e1 env) < (interp fundefs e2 env)
    then 1 else 0
  | If(cond,then_exp,else_exp) ->
    if (interp fundefs cond env) = 0
    then (interp fundefs else_exp env)
    else (interp fundefs then_exp env)
  | Call(funname, arg_exps) | TCall(funname, arg_exps) ->
    let arg_vals = List.map (fun e -> interp fundefs e env) arg_exps in
    let {args; body} = List.find (fun{name}-> name=funname) fundefs in
    interp fundefs body (make_env args arg_vals)
  | Let(var,exp,body)  ->
    interp fundefs body
      (extend_env env var (interp fundefs exp env))

(* test *)
let _ =
  print_int (interp [fact] (Call("fact",[Int 3])) null_env) ;
  print_int (interp [fib] (Call("fib",[Int 8])) null_env) ;

  (* virtual machine *)
module VM : sig
  (* instruction set: a stack machine *)
  type inst =
    | ADD                       (* n2::n1::s -> (n1+n2)::s *)
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
    (* the following constructors do not represent instructions but
       are defined for expressing operands of some instructions as
       well as label declarations and references *)
    | Literal of int
    | Lref of string
    | Ldef of string

  val string_of : inst -> string (* for debugging *)

  type fundef_bin_t = int array 
  type fundef_asm_t = inst array 
  val run_bin : fundef_bin_t -> int
  val run_asm : fundef_asm_t -> int
end = struct
  type inst =
    | ADD
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
    | Literal of int
    | Lref of string
    | Ldef of string

  let max_stack_depth = 1024

  (* the next array determines the opcode *)
  let insts = [| ADD; MUL; LT; CONST; JUMP_IF_ZERO; (* LOAD; STORE; *) CALL; RET;
                 DUP; HALT; FRAME_RESET; POP1 |]

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

let list_init n f =
  let rec loop i =
    if i=n then []
    else (f i)::(loop (i+1)) in
  loop 0

(* a simple compiler *)
module Compiler : sig
  val compile_funs : fundef list -> VM.inst array 

  (* for tracing *)
  val compile_fun : (var -> int) -> fundef -> VM.inst list
end = struct

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
        | Mul(e1,e2) -> (compile_exp fenv e1 env) @ 
                        (compile_exp fenv e2 (shift_env env)) @ [MUL]
        | LT(e1,e2) -> (compile_exp fenv e1 env) @ 
                       (compile_exp fenv e2 (shift_env env)) @ [LT]
        | If(cond,then_exp,else_exp) ->
          let l2,l1 = gen_label(),gen_label() in
          (compile_exp fenv cond env) 
          @ [JUMP_IF_ZERO; Lref l1]
          @ (compile_exp fenv then_exp env)
          @ [CONST; Literal 0;         (* unconditional jump *)
             JUMP_IF_ZERO; Lref l2;
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
             CONST; Literal 0; JUMP_IF_ZERO; Lref fname]
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

  let compile_fun_body fenv name arity exp env =
    (VM.Ldef name)::(compile_exp fenv exp env) @ [VM.RET; VM.Literal arity]

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

  (* for testing *)
  let string_of_code_list clist =
    String.concat ";" (List.map VM.string_of clist)
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
  open VM
  let _ = test "Int" (Int 123) [CONST; Literal 123]
  let _ = test "Var" (Var "y") [DUP; Literal 2]
  let _ = test "Add" (Add(Var "x",Var "x"))
      [DUP; Literal 3; DUP; Literal 4; ADD]
  let _ = test "Mul" (Mul(Int 123,Int 456))
      [CONST; Literal 123; CONST; Literal 456; MUL]
  let _ = test "Lt" (LT(Int 123,Int 456))
      [CONST; Literal 123; CONST; Literal 456; LT]
  let _ = test "If" (If(Int 123,Int 456,Int 789))
      [CONST; Literal 123; JUMP_IF_ZERO; Lref "$0";
       CONST; Literal 456; CONST; Literal 0; JUMP_IF_ZERO; Lref "$1";
       Ldef "$0";
       CONST; Literal 789; Ldef "$1"]
  let _ = test "Call" (Call("f",[Var "x";Var "x"]))
      [DUP; Literal 3; 
       DUP; Literal 4; CALL; Lref "f"]
  let _ = test "Let" (Let("z",Int 123,Add(Var "z",Var "y")))
      [CONST; Literal 123; 
       DUP; Literal 0;
       DUP; Literal 4;
       ADD;
       POP1]
  let _ = test "TCall" (TCall("f",[Int 123;Int 456;Int 789]))
      [CONST; Literal 123; 
       CONST; Literal 456;
       CONST; Literal 789;
       FRAME_RESET; Literal 2; Literal 1; Literal 3;
       CONST; Literal 0; JUMP_IF_ZERO; Lref "f"]

  let _ = Printf.printf "%b"
      ((make_label_env [VM.ADD; VM.Ldef "foo"]) = [Lref "foo", Literal 2])
  let _ = test_ex (fun fenv exp env -> resolve_labels (compile_exp fenv exp env))
      "resolve"
      (If(Int 123,Int 456,Int 789))
      [CONST; Literal 123; (* 0;1 *)
       JUMP_IF_ZERO; Literal 10; (* 2;3 *)
       CONST; Literal 456;   (* 4;5 *)
       CONST; Literal 0;     (* 6;7 *)
       JUMP_IF_ZERO; Literal 12; (* 8;9 *)
       CONST; Literal 789;   (* 10;11 *)
      ]

  let elim_test name src expected =
    let elimed = tail_elim "f" src in
    if elimed = expected
    then Printf.printf "ELIM %s : OK\n" name
    else Printf.printf "ELIM %s : NG\n" name

  let callf,tcallf = Call("f", []), TCall("f", [])
  let callg a = Call("g", a)
  let _ = elim_test "top"  callf tcallf  
  let _ = elim_test "other" (callg []) (callg [])
  let _ = elim_test "not top" (callg [callf]) (callg [callf]) 
  let _ = elim_test "if" (If(callf,callf,callf)) (If(callf,tcallf,tcallf))
  let _ = elim_test "let" (Let("v",callf,callf)) (Let("v",callf,tcallf))


end

let main name args =
  {name="main"; args=[]; body=Call(name,List.map (fun n ->Int n) args)}

let ccexe funs fname args =
  let funs = (main fname args)::funs in
  let obj = Compiler.compile_funs funs in
  let result = VM.run_asm obj in
  Printf.printf "%s %s => %d\n"
    fname (String.concat " " (List.map string_of_int args))
    result; result

(* some more test programs *)

let let_test = {name="let_test"; args=[];
                body=Add(Int 456,Let("x",Int 123,Var "x"))}
let _ = assert (456+123 = ccexe [let_test] "let_test" [])

(* let _ = assert (3628800 = ccexe [fact] "fact" [10])
 * let _ = assert (6765 = ccexe [fib]  "fib" [20]) *)

let rec sum_ocaml n =
  if n=0 then 0 else n+(sum_ocaml (n-1))

let sum = {name="sum"; args=["n"];
           body=If(LT(Var "n", Int 1), Int 0,
                   Add(Var "n", Call("sum",[Add(Var "n", Int(-1))])))}
(* let _ = assert (55 = ccexe [sum] "sum" [10]) *)

let rec gcd_ocaml(a,b) =
  if       a=b then a
  else (if a<b then gcd_ocaml(a,b-a)
        else gcd_ocaml(a-b,b))
let eq  = {name="eq"; args=["x"; "y"];
           body=If(LT(Var "x", Var "y"), Int 0,
                   If(LT(Var "y", Var "x"), Int 0, Int 1))}
let sub = {name="sub"; args=["x"; "y"];
           body=Add(Var "x", Mul(Var "y", Int(-1)))}
let gcd = {name="gcd"; args=["a"; "b"];
           body=If(Call("eq", [Var "a"; Var "b"]),
                   Var "a",
                   If(LT(Var "a", Var "b"),
                      Call("gcd",[Var "a"; Call("sub", [Var "b"; Var "a"])]),
                      Call("gcd",[Call("sub", [Var "a"; Var "b"]); Var "b"])))}

(* compliation of gcd yields the following bytecode (local variable 
   indices, function indices are replaced with variable names like a, b, and sub
   for readability.   But the key points are, the tail calls are eliminated
   into jump-to-the-beginning instructions (i.e., CONST 0; JUMP_IF_ZERO 1).

   [|Literal 2; 
   1:LOAD a;
    LOAD b;
    CALL eq;
    JUMP_IF_ZERO 15;
    LOAD a;
    CONST 0; JUMP_IF_ZERO 58;
   15:LOAD a;
    LOAD b;
    LT;
    JUMP_IF_ZERO 42;
    LOAD a;
    LOAD b;
    LOAD a;
    CALL sub;
    STORE b;
    STORE a;
    CONST 0; JUMP_IF_ZERO 1;
    CONST 0; JUMP_IF_ZERO 58;
   42:LOAD a;
    LOAD b;
    CALL sub;
    LOAD b;
    STORE b;
    STORE a;
    CONST 0; JUMP_IF_ZERO 1;
   58:RET|]
*)            

(* let _ = assert (21 = ccexe [eq;sub;gcd] "gcd" [252;105]) *)

let rec mod_ n m =
  if n < m then n
  else mod_ (n-m) m
let rec prime_test cand i =
  let i2 = i*i in
  if cand < i2 then 1
  else if mod_ cand i = 0 then 0
  else prime_test cand (i+1)
let is_prime cand =
  if 2<cand then prime_test cand 2 else 1

let mod_ = {name="mod"; args=["n";"m"];
            body=If(LT(Var "n", Var "m"), Var "n",
                    Call("mod", [Call("sub", [Var "n"; Var "m"]);
                                 Var "m"]))}
let prime_test = {name="prime_test"; args=["cand"; "i"];
                  body=Let("i2", Mul(Var "i", Var "i"),
                           If(LT(Var "cand", Var "i2"),
                              Int 1,
                              (If(Call("eq",[Call("mod", [Var "cand"; Var "i"]);
                                             Int 0]),
                                  Int 0,
                                  Call("prime_test",
                                       [Var "cand"; Add(Var "i", Int 1)])))))}

let is_prime = {name="is_prime"; args=["cand"];
                body=If(LT(Int 2, Var "cand"),
                        Call("prime_test", [Var "cand"; Int 2]),
                        Int 1)}

(* let _ = assert (1 = ccexe [sub;eq;mod_;prime_test;is_prime] "mod" [7;3]) *)
let _ = print_string "start prime_test\n"
(* let _ = assert (1 = ccexe [sub;eq;mod_;prime_test;is_prime] "prime_test" [3;2]) *)

let _ = assert (1 = ccexe [sub;eq;mod_;prime_test;is_prime] "is_prime" [7])
let _ = assert (0 = ccexe [sub;eq;mod_;prime_test;is_prime] "is_prime" [9])
let _ = assert (1 = ccexe [sub;eq;mod_;prime_test;is_prime] "is_prime" [83])
(* let _ = assert (1 = ccexe [sub;eq;mod_;prime_test;is_prime] "is_prime" [967]) *)
