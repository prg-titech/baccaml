open Syntax

let stack_hybridized = ref true

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
  match List.find_opt (fun (_,v) -> var = v)
          (List.mapi (fun idx v -> (idx,v)) env) with
  | Some v -> fst v
  | None -> failwith (Printf.sprintf "%s not found" var)
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
let rec compile_exp fenv env exp =
  let open VM in
  match exp with
  | Unit -> []
  | Int n -> [CONST; Literal n]
  | Not e1 ->
    (e1 |> compile_exp fenv env) @
    [NOT]
  | Var v -> [DUP; Literal(lookup env v)]
  | Add(e1,e2) ->
    (e1 |> compile_exp fenv env) @
    (e2 |> compile_exp fenv (shift_env env)) @ [ADD]
  | Sub(e1, e2) ->
    (compile_exp fenv env e1) @
    (compile_exp fenv (shift_env env) e2) @ [SUB]
  | Mul(e1,e2) ->
    (e1 |> compile_exp fenv env) @
    (e2 |> compile_exp fenv (shift_env env)) @ [MUL]
  | LT(e1,e2) ->
    (e1 |> compile_exp fenv env) @
    (e2 |> compile_exp fenv (shift_env env)) @ [LT]
  | Eq(e1, e2) ->
    (e1 |> compile_exp fenv env) @
    (e2 |> compile_exp fenv (shift_env env)) @ [EQ]
  | If(cond,then_exp,else_exp) ->
    let l2,l1 = gen_label(),gen_label() in
    (cond |> compile_exp fenv env)
    @ [JUMP_IF_ZERO; Lref l1]
    @ (then_exp |> compile_exp fenv env)
    @ [JUMP; Lref l2; (* unconditional jump *)
       Ldef l1]
    @ (else_exp |> compile_exp fenv env)
    @ [Ldef l2]
  | Call(annot, fname, rands) | TCall(annot, fname, rands) ->
    (List.fold_left (fun (rev_code_list,env) exp ->
         (exp |> compile_exp fenv env) :: rev_code_list, shift_env env)
        ([], env) rands
     |> fst
     |> List.rev
     |> List.flatten) @
    begin
      match annot with
      | Some MethodComp ->
        [CALL_HS; Lref fname; Literal (List.length rands)]
      | _ ->
        [CALL; Lref fname; Literal (List.length rands)]
    end
  (* call using a label (call label size_of_args) *)
  (* self tail calls are compiled with FRAME_RESET which moves
         the computed arguments to the position of the actual
         parameters in the current frame. *)
  | TCall(annot, fname, rands) ->
    let old_arity,local_size = arity_of_env env in
    let new_arity = List.length rands in
    (List.flatten
       (List.rev
          (fst
             (List.fold_left (fun (rev_code_list,env) exp ->
                  (exp |> compile_exp fenv env)::rev_code_list,
                  shift_env env)
                 ([], env) rands))))
    @ [FRAME_RESET;
       Literal old_arity; Literal local_size; Literal new_arity;
       JUMP; Lref fname]
  | Let(var,exp,body) ->
    let ex_env = extend_env env var in
    (exp |> compile_exp fenv env)         (* in old env *)
    @ (body |> compile_exp fenv ex_env)   (* in extended env *)
    @ [POP1]                              (* drop the value *)
  | Array (e1, e2) ->
    (e1 |> compile_exp fenv env) @
    (e2 |> compile_exp fenv (shift_env env)) @
    [ARRAY_MAKE]
  | Get (e1, e2) ->
    (e1 |> compile_exp fenv env) @
    (e2 |> compile_exp fenv (shift_env env)) @
    [GET]
  | Put (e1, e2, e3, e4) ->       (* array,index,val,cont *)
    (e1 |> compile_exp fenv env)
    @ (e2 |> compile_exp fenv (shift_env env))
    @ (e3 |> compile_exp fenv (shift_env (shift_env env)))
    @ [PUT]
    @ (e4 |> compile_exp fenv env)
  (* for var = from_exp to to_exp do
   *   body_exp
   * done;
   * next_exp *)
  | For (Range (var, from_exp, to_exp), body_exp, next_exp) ->
    let l1 = gen_label () in
    let ex_env = extend_env env var in      (* var *)
    (from_exp |> compile_exp fenv env) @
    [Ldef l1] @
    (body_exp |> compile_exp fenv ex_env) @ (* body of loop *)
    [CONST; Literal 1; ADD] @               (* increment var *)
    [DUP; Literal (lookup ex_env var)] @    (* copy var *)
    (to_exp |> compile_exp fenv ex_env) @
    [LT; NOT] @                             (* compaire with to *)
    [JUMP_IF_ZERO; Lref l1] @
    [POP0] @                                (* remove var *)
    (next_exp |> compile_exp fenv env)
  | _ -> failwith (Printf.sprintf "match failure %s" (Syntax.show_exp exp))

let rec tail_elim fname = function
  | If(cond,then_exp,else_exp) ->
    If(cond, tail_elim fname then_exp, tail_elim fname else_exp)
  | Call(annot, fname', rands) ->
    if fname=fname'
    then TCall(annot, fname', rands)
    else Call(annot, fname', rands)
  | Let(var,exp,body) ->
    Let(var,exp, tail_elim fname body)
  | others -> others


let rec call_annot (fenv : var -> fundef) = function
  | If(cond, then_exp, else_exp) ->
    If(cond, call_annot fenv then_exp, call_annot fenv else_exp)
  | Call(annot', fname', rands) ->
    let callee = fenv fname' in
    (match callee.annot with
     | Some MethodComp -> Call(Some MethodComp, fname', rands)
     | _ -> Call(annot', fname', rands))
  | Let(var,exp,body) ->
    Let(var, call_annot fenv exp, call_annot fenv body)
  | For(range, body_exp, next_exp) ->
    For(range, call_annot fenv body_exp, call_annot fenv next_exp)
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


let compile_fun_body fenv name arity annot exp env =
  let env = if !stack_hybridized then shift_env env else env in
  (match annot with
   | None | Some TracingComp -> [VM.METHOD_ENTRY; VM.Ldef name]
   | Some MethodComp -> [VM.METHOD_COMP; VM.METHOD_ENTRY; VM.Ldef name]) @
  (call_annot fenv exp |> compile_exp fenv env) @
  (if name = "main"
   then [VM.HALT]
   else [VM.RET; VM.Literal arity])


let compile_fun (fenv : var -> fundef) {name; args; body; annot} =
  compile_fun_body fenv name (List.length args) annot
    (tail_elim name body)
    (build_arg_env args)


let compile_funs fundefs =
  (* let fenv name = fst(List.find (fun (_,{name=n}) -> name=n)
   *                       (List.mapi (fun idx fdef -> (idx,fdef))
   *                          fundefs)) in *)
  let fenv name = List.find (fun {name=n} -> n=name) fundefs in
  Array.of_list(resolve_labels
                  (List.flatten
                     (List.map (compile_fun fenv) fundefs)))


let compile_from_exp (exp : Syntax.exp) : VM.inst array =
  let fundefs = find_fundefs exp in
  let main = fundefs |> List.find_all (fun { name } -> name = "main") in
  let others = fundefs |> List.filter (fun { name } -> name <> "main") in
  (compile_funs (others @ main))


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
    let actual = exp |> compiler (fun _ -> 47)
                   (extend_env (build_arg_env ["x";"y"]) "w") in
    if actual = expected
    then (Printf.printf "%s: %s\n" name "OK")
    else failwith (Printf.sprintf "%s: NG\n%s"
                     name (print_code_pair expected actual))
  let test = test_ex compile_exp

  let compile_from_exp (exp : Syntax.exp) : VM.inst array =
    let fundefs = find_fundefs exp in
    let main = fundefs |> List.find (fun { name } -> name = "main") in
    let others = fundefs |> List.filter (fun { name } -> name <> "main") in
    (compile_funs (main :: others))
end

let%test_module "compiler_test" = (module struct
  open Syntax
  open VM
end)
