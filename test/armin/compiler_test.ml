open MyFront
open Syntax
open Compiler

module VM = Virtual

let string_of_code_list clist =
  String.concat ";" (List.map VM.string_of clist)

let print_code_pair code1 code2 =
  Printf.sprintf "code1: %s\ncode2: %s\n"
    (string_of_code_list code1) (string_of_code_list code2)

let test_ex compiler name exp expected =
  let actual = compiler (fun _ -> 47) exp
      (extend_env (build_args_env ["x";"y"]) "w") in
  if actual = expected
  then (Printf.printf "%s: %s\n" name "OK")
  else failwith (Printf.sprintf "%s: NG\n%s"
                   name (print_code_pair expected actual))

let test = test_ex compile_exp

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

let main name args =
  { name = "main";
    args = [];
    body = Call(name, List.map (fun n ->Int n) args)
  }

let ccexe funs fname args =
  let funs = (main fname args) :: funs in
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

let is_prime = {name = "is_prime"; args = ["cand"];
                body = If ( LT (Int 2, Var "cand"),
                        Call ("prime_test", [Var "cand"; Int 2]),
                        Int 1)}

(* let _ = assert (1 = ccexe [sub;eq;mod_;prime_test;is_prime] "mod" [7;3]) *)
let _ = print_string "start prime_test\n"
(* let _ = assert (1 = ccexe [sub;eq;mod_;prime_test;is_prime] "prime_test" [3;2]) *)

let _ = assert (1 = ccexe [sub;eq;mod_;prime_test;is_prime] "is_prime" [7])
let _ = assert (0 = ccexe [sub;eq;mod_;prime_test;is_prime] "is_prime" [9])
let _ = assert (1 = ccexe [sub;eq;mod_;prime_test;is_prime] "is_prime" [83])
(* let _ = assert (1 = ccexe [sub;eq;mod_;prime_test;is_prime] "is_prime" [967]) *)
