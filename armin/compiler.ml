open Syntax
open Virtual

module VM = Virtual

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
  fst(List.find (fun (_,v) -> var = v)
        (List.mapi (fun idx v -> (idx, v)) env))

let empty_env = []
let extend_env env var = var :: env
let shift_env env = extend_env env "*dummy*"

let empty_fenv = (fun _ -> 47)
let extend_fenv fenv fundef = fundef :: fenv

let return_address_marker = "$ret_addr"
let build_args_env args = return_address_marker :: (List.rev args)
(* computes the number of arguments to this frame.  The stack has a
   shape like [...local vars...][ret addr][..args...], the return
   address position from the top indicates the number of local
   variables on top of the return address. *)
let arity_of_env env =
  let num_local_vars = lookup env return_address_marker in
  (List.length env - num_local_vars  - 1, num_local_vars)

let rec tail_elim fname = function
  | If (cond,then_exp,else_exp) ->
    If (cond, tail_elim fname then_exp, tail_elim fname else_exp)
  | Call(fname', rands) ->
    if fname = fname'
    then TCall (fname', rands)
    else Call (fname', rands)
  | Let (var,exp,body) ->
    Let (var,exp, tail_elim fname body)
  | others -> others

(* [...;Ldef a;...] -> [...;a,i;...] where i is the index of the
   next instruction of Ldef a in the list all Ldefs are removed
   e.g., [_;Ldef 8;_;Ldef 7;_] ==> [8,1; 7,2]
*)
let make_label_env instrs =
  snd(List.fold_left
        (fun (addr,env) -> function
           | Ldef n -> (addr, (Lref n, Literal(addr))::env)
           | _ ->      (addr+1, env)) (0,[]) instrs)

(* resolving labels *)
let assoc_if subst elm =
  try List.assoc elm subst with
  | Not_found  -> elm

(* remove all Ldefs and replace Lrefs with Literals *)
let resolve_labels instrs =
  List.filter (function Ldef _ -> false | _ -> true)
    (List.map (assoc_if (make_label_env instrs)) instrs)

let rec compile_exp_entry fenv exp env =
  let exps = compile_exp fenv exp env in
  match List.find_opt (fun inst -> inst = HALT) exps with
  | Some _ -> exps
  | None -> exps @ [HALT]

(* compilation of expressions *)
and compile_exp fenv exp env =
  match exp with
  | Int n -> [CONST; Literal n]
  | Var v -> [DUP; Literal(lookup env v)]
  | Add (e1, e2) -> (compile_exp fenv e1 env) @
                    (compile_exp fenv e2 (shift_env env)) @ [ADD]
  | Sub (e1, e2) -> (compile_exp fenv e1 env) @
                    (compile_exp fenv e2 (shift_env env)) @ [SUB]
  | Mul (e1, e2) -> (compile_exp fenv e1 env) @
                    (compile_exp fenv e2 (shift_env env)) @ [MUL]
  | LT (e1, e2) -> (compile_exp fenv e1 env) @
                   (compile_exp fenv e2 (shift_env env)) @ [LT]
  | If (cond, then_exp, else_exp) ->
    let l2, l1 = gen_label (), gen_label () in
    (compile_exp fenv cond env) @
    [JUMP_IF_ZERO; Lref l1] @
    (compile_exp fenv then_exp env) @
    [JUMP; Lref l2;
     Ldef l1] @
    (compile_exp fenv else_exp env) @
    [Ldef l2]
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
  | TCall (fname, rands) ->
    let old_arity,local_size = arity_of_env env in
    let new_arity = List.length rands in
    (List.flatten
       (List.rev
          (fst
             (List.fold_left (fun (rev_code_list,env) exp ->
                  (compile_exp fenv exp env)::rev_code_list,
                  shift_env env)
                 ([], env) rands)))) @
    [FRAME_RESET;
     Literal old_arity; Literal local_size; Literal new_arity;
     CONST; Literal 0; JUMP_IF_ZERO; Lref fname]
  | Let (var, exp, body) ->
    let ex_env = extend_env env var in
    (compile_exp fenv exp env) @         (* in old env *)
    (compile_exp fenv body ex_env) @     (* in extended env *)
    [POP1]                               (* drop the value *)
  | LetRec (fundef, body) ->
    (compile_fun fenv fundef) @
    (compile_exp fenv body env) @
    [HALT]

and compile_funs fundefs =
  let fenv name =
    fst (List.find (fun (_, { name = n; _ }) -> name = n)
           (List.mapi (fun idx fdef -> (idx, fdef))
              fundefs)) in
  Array.of_list(resolve_labels
                  (List.flatten
                     (List.map (compile_fun fenv) fundefs)))

and compile_fun fenv { name; args; body } =
  compile_fun_body fenv name (List.length args)
    (tail_elim name body)
    (build_args_env args)

and compile_fun_body fenv name arity exp env =
  (VM.Ldef name) :: (compile_exp fenv exp env) @ [VM.RET; VM.Literal arity]
