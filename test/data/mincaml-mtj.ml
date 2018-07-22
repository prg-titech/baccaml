(* simple compiler and bytecode interpreter *)

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
  (* instruction set: a stack machine with local variables *)
  type inst =
    | ADD                       (* n2::n1::s -> (n1+n2)::s *)
    | MUL                       (* n2::n1::s -> (n1*n2)::s *)
    | LT                        (* n2::n1::s -> (n1<n2)::s *)
    | CONST (* n *)             (* s         -> n::s *)
    | JUMP_IF_ZERO (* addr *)   (* n::s      -> s *)
    | LOAD (* n *)              (* s         -> reg_n.s *)
    | STORE (* n *)             (* n::s      -> s *)
    | CALL (* fun-id *)         (* nm::...::n1::s -> v::s *)
    | RET                       (* n         -> . *)
    (* the following constructors do not represent instructions but
       are defined for expressing operands of some instructions as
       well as label declarations and references *)
    | Literal of int
    | Lref of int
    | Ldef of int

  val string_of : inst -> string (* for debugging *)

  type fundef_bin_t = int array array
  type fundef_asm_t = inst array array
  val run_bin : fundef_bin_t -> int
  val run_asm : fundef_asm_t -> int
end = struct
  type inst =
    | ADD
    | MUL
    | LT
    | CONST
    | JUMP_IF_ZERO (* addr *)
    | LOAD (* n *)
    | STORE (* n *)
    | CALL (* fun-id *)
    | RET
    | Literal of int
    | Lref of int
    | Ldef of int

  (* the next array determines the opcode *)
  let insts = [| ADD; MUL; LT; CONST; JUMP_IF_ZERO; LOAD; STORE; CALL; RET |]

  let index_of element array =
    fst(List.find (fun (_,v) -> v=element)
          (List.mapi (fun idx v -> (idx,v)) (Array.to_list array)) )
  let int_of_inst = function
    | Literal n -> n
    | inst -> index_of inst insts
  let string_of = function
    | Literal n -> Printf.sprintf "Literal %d" n
    | Ldef n    -> Printf.sprintf "Ldef %d" n
    | Lref n    -> Printf.sprintf "Lref %d" n 
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

  let make_stack () = (0, Array.make 64 0)

  (* construct a local variable set from values in an operand stack *)
  let make_lvars arity stack =
    let lvars = Array.make 256 0 in
    let rec loop i stack =
      if i = 0 then (lvars,stack)
      else let v,stack = pop stack in
        (lvars.(i-1) <- v;
         loop (i-1) stack) in
    loop arity stack

  (* fetch one integer from the code, and advance the program counter *)
  let fetch code pc = (code.(pc), pc+1)

  let rec interp fundefs code pc stack lvars =
    (* Printf.printf "fun_id=%d pc=%d sp=%d ostack(sp-1)=%d\n"
     *               pc sp (if 0<sp then (ostack.(sp-1)) else -1); *)
    let sp,ostack = stack in
    let i,pc = fetch code pc in
    match insts.(i) with 
    | ADD -> let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (v1+v2) in
      interp fundefs code pc stack lvars
    | MUL -> let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (v1*v2) in
      interp fundefs code pc stack lvars
    | LT ->  let v2,stack = pop stack in
      let v1,stack = pop stack in
      let    stack = push stack (if v1<v2 then 1 else 0) in
      interp fundefs code pc (sp-1, ostack) lvars
    | CONST -> let c,pc = fetch code pc in
      let stack = push stack c in
      interp fundefs code pc stack lvars
    | JUMP_IF_ZERO (* addr *) ->
      let addr,pc = fetch code pc in
      let v,stack = pop stack in 
      interp fundefs code (if v=0 then addr else pc) stack lvars
    | LOAD (* n *) ->
      let n,pc = fetch code pc in
      let stack = push stack (lvars.(n)) in 
      interp fundefs code pc stack lvars
    | STORE (* n *) ->
      let n,pc = fetch code pc in
      let v,stack = pop stack in
      lvars.(n) <- v;
      interp fundefs code pc stack lvars
    | CALL (* function_id *) ->
      (* calling a function will create a new operand stack and lvars  *)
      let target_id,pc = fetch code pc  in
      let target_code = fundefs.(target_id) in
      let arity = target_code.(0) in
      let new_lvars,stack = make_lvars arity stack in
      let v = interp fundefs target_code 1 (make_stack ()) new_lvars in
      let stack = push stack v in
      interp fundefs code pc stack lvars
    | RET ->
      (* when returning from a function the operand stack should
         contain one and only one value *)
      if sp=1
      then fst(pop stack)
      else raise (Failure "not one stack at RET")

  (* run the given program by calling the function id 0 *)
  type fundef_bin_t = int array array
  let run_bin : fundef_bin_t -> int = fun fundefs ->
    let target_code = fundefs.(0) in
    let arity = target_code.(0) in
    let stack = make_stack () in
    let lvars,_ = (make_lvars arity stack) in
    if arity = 0 then
      interp fundefs target_code 1 stack lvars
    else
      raise (Failure "function 0 should be nullary.")

  (* convert the given program into binary, and then run *)
  type fundef_asm_t = inst array array
  let run_asm : fundef_asm_t -> int = fun fundefs ->
    run_bin (Array.map (Array.map int_of_inst) fundefs)

  let test_ex arity pc name instrs ostack lvars expected =
    let code = Array.of_list (arity::(List.map int_of_inst instrs)) in
    let sp = List.length ostack in
    let ostack = Array.init 64 (fun i ->
        if i < sp then List.nth ostack i else 0) in
    let lvars = Array.init 256 (fun i ->
        if i < List.length lvars 
        then List.nth lvars i else 0) in
    let v = interp [| code |] code pc (sp, ostack) lvars in
    if v=expected
    then (Printf.printf "OK: %s\n" name)
    else (Printf.printf "NG: %s expected=%d actual=%d\n" name expected v )
  let test = test_ex 0 1

  let _ = test "ADD" [ADD; RET] [ 1; 2 ] [] 3
  let _ = test "MUL" [MUL; RET] [ 2; 3 ] [] 6
  let _ = test "LT true" [LT; RET] [ 2; 3 ] [] 1
  let _ = test "LT false" [LT; RET] [ 3; 2 ] [] 0
  let _ = test "CONST" [CONST; Literal 3; RET] [ ] [] 3
  let _ = test "JUMP_IF_ZERO taken"
      [JUMP_IF_ZERO; Literal 6; (* 1;2 *)
       CONST; Literal 1;        (* 3;4 *)
       RET;                     (* 5 *)
       CONST; Literal 2;        (* 6;7 *)
       RET] [ 0 ] [] 2
  let _ = test "JUMP_IF_ZERO not taken"
      [JUMP_IF_ZERO; Literal 6; (* 1;2 *)
       CONST; Literal 1;        (* 3;4 *)
       RET;                     (* 5 *)
       CONST; Literal 2;        (* 6;7 *)
       RET] [ 8 ] [] 1
  let _ = test "LOAD" [LOAD; Literal 0; RET] [] [ 1 ] 1
  let _ = test "STORE" [STORE; Literal 0;
                        LOAD; Literal 0; RET] [3] [ ] 3
  let _ = test_ex 2 4 "CALL simple"
      [LOAD; Literal 0; (* 1;2 *)
       RET;             (* 3 *)
       CALL; Literal 0;  (* 4;5 *)
       RET]              (* 6 *)
      [ 9;8 ] [] 9
  let _ = test_ex 2 7 "CALL mul add"
      [LOAD; Literal 0; (* 1;2 *)
       LOAD; Literal 1; (* 3;4 *)
       MUL;             (* 5 *)
       RET;             (* 6 *)
       CALL; Literal 0;  (* 7;8 *)
       ADD;              (* 9 *)
       RET]              (* 10 *)
      [ 1; 2; 3 ] [] 7
end

let list_init n f =
  let rec loop i =
    if i=n then []
    else (f i)::(loop (i+1)) in
  loop 0

(* a simple compiler *)
module Compiler : sig
  val compile_funs : fundef list -> VM.inst array array

  (* for tracing *)
  val compile_fun : (var -> int) -> fundef -> VM.inst array
end = struct

  (* generate a unique label id *)
  let gen_label, reset =
    let counter = ref 0 in
    ((fun () -> let l = !counter in
       counter := (!counter + 1);
       l),
     fun () -> counter := 0)

  (* compilation environment maps local variable names to local
     variable numbers *)
  let lookup env var =
    let Some idx =
      List.fold_left
        (function 
          | Some n -> (fun _ -> Some (n+1))
          | None   -> (fun v ->
              if v=var then Some 0 else None)) None env in
    idx
  let extend_env env var = var :: env

  (* compilation of expressions *)
  let rec compile_exp fenv exp env =
    VM.(match exp with
        | Int n -> [CONST; Literal n]
        | Var v -> [LOAD; Literal(lookup env v)]
        | Add(e1,e2) -> (compile_exp fenv e1 env) @ 
                        (compile_exp fenv e2 env) @ [ADD]
        | Mul(e1,e2) -> (compile_exp fenv e1 env) @ 
                        (compile_exp fenv e2 env) @ [MUL]
        | LT(e1,e2) -> (compile_exp fenv e1 env) @ 
                       (compile_exp fenv e2 env) @ [LT]
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
             (List.map (fun exp -> compile_exp fenv exp env) rands)) @
          [CALL; Literal(fenv fname)]
        (* self tail calls are compiled into a series of store
           followed by a jump to zero.  This compilation assumes that
           only self-tail calls are using TCall node. *)
        | TCall(fname, rands) ->
          let arity = List.length rands in
          (List.flatten
             (List.map (fun exp -> compile_exp fenv exp env) rands))
          @ (List.flatten
               (list_init arity (fun i -> [STORE; Literal (arity-i-1)])))
          @ [CONST; Literal 0; JUMP_IF_ZERO; Literal 1]
        | Let(var,exp,body) ->
          let ex_env = extend_env env var in
          (compile_exp fenv exp env)            (* in old env *)
          @ [STORE; Literal(lookup ex_env var)] (* in extended env *)
          @ (compile_exp fenv body ex_env)      (* in extended env *)
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
              | _ ->      (addr+1, env)) (1,[]) instrs)

  (* remove all Ldefs and replace Lrefs with Literals *)
  let resolve_labels instrs =
    List.filter (function VM.Ldef _ -> false | _ -> true) 
      (List.map (assoc_if (make_label_env instrs)) instrs)

  let compile_fun_body fenv arity exp env =
    Array.of_list ((VM.Literal arity) ::
                   resolve_labels (compile_exp fenv exp env) @ 
                   [VM.RET])

  let compile_fun fenv {name; args; body} =
    compile_fun_body fenv (List.length args)
      (tail_elim name body)
      (List.rev args)

  let compile_funs fundefs =
    let fenv name = fst(List.find (fun (_,{name=n}) -> name=n)
                          (List.mapi (fun idx fdef -> (idx,fdef))
                             fundefs)) in
    Array.of_list(List.map (compile_fun fenv) fundefs)

  (* for testing *)
  let string_of_code_list clist =
    String.concat ";" (List.map VM.string_of clist)
  let print_code_pair code1 code2 =
    Printf.sprintf "code1: %s\ncode2: %s\n"
      (string_of_code_list code1) (string_of_code_list code2)
  let test_ex compiler name exp expected =
    let actual = compiler (fun _ -> 47) exp ["y";"x"] in
    if actual = expected
    then (Printf.printf "%s: %s\n" name "OK")
    else (Printf.printf "%s: NG\n%s"
            name (print_code_pair expected actual))
  let test = test_ex compile_exp
  open VM
  let _ = test "Int" (Int 123) [CONST; Literal 123]
  let _ = test "Var" (Var "y") [LOAD; Literal 1]
  let _ = test "Add" (Add(Int 123,Int 456))
      [CONST; Literal 123; CONST; Literal 456; ADD]
  let _ = test "Mul" (Mul(Int 123,Int 456))
      [CONST; Literal 123; CONST; Literal 456; MUL]
  let _ = test "Lt" (LT(Int 123,Int 456))
      [CONST; Literal 123; CONST; Literal 456; LT]
  let _ = test "If" (If(Int 123,Int 456,Int 789))
      [CONST; Literal 123; JUMP_IF_ZERO; Lref 0;
       CONST; Literal 456; CONST; Literal 0; JUMP_IF_ZERO; Lref 1;
       Ldef 0;
       CONST; Literal 789; Ldef 1]
  let _ = test "Call" (Call("f",[Int 123;Int 456]))
      [CONST; Literal 123; 
       CONST; Literal 456; CALL; Literal 47]
  let _ = test "Let" (Let("z",Int 123,Var "z"))
      [CONST; Literal 123; 
       STORE; Literal 2; LOAD; Literal 2]
  let _ = test "TCall" (TCall("f",[Int 123;Int 456]))
      [CONST; Literal 123; 
       CONST; Literal 456;
       STORE; Literal 1;
       STORE; Literal 0;
       CONST; Literal 0; JUMP_IF_ZERO; Literal 1]

  let _ = Printf.printf "%b"
      ((make_label_env [VM.ADD; VM.Ldef 0]) = [Lref 0, Literal 2])
  let _ = test_ex (fun fenv exp env -> resolve_labels (compile_exp fenv exp env))
      "resolve"
      (If(Int 123,Int 456,Int 789))
      [CONST; Literal 123; (* 1;2 *)
       JUMP_IF_ZERO; Literal 11; (* 3;4 *)
       CONST; Literal 456;   (* 5;6 *)
       CONST; Literal 0;     (* 7;8 *)
       JUMP_IF_ZERO; Literal 13; (* 9;10 *)
       CONST; Literal 789;   (* 11;12 *)
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

let _ = ccexe [fact] "fact" [10]
let _ = ccexe [fib]  "fib" [20]

let rec sum_ocaml n =
  if n=0 then 0 else n+(sum_ocaml (n-1))

let sum = {name="sum"; args=["n"];
           body=If(LT(Var "n", Int 1), Int 0,
                   Add(Var "n", Call("sum",[Add(Var "n", Int(-1))])))}
let _ = ccexe [sum] "sum" [10]

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

let _ = ccexe [eq;sub;gcd] "gcd" [252;105]

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

let _ = ccexe [sub;eq;mod_;prime_test;is_prime] "is_prime" [967]

