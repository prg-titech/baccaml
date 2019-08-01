
open Std
open Base
open Asm
open Inlining
open Renaming
open Operands
open Jit_util
open Jit_env

let find_pc {index_pc} args =
  match List.nth_opt args index_pc with
  | Some s -> int_of_id_t s
  | None -> failwith "find_pc is failed"

let find_value reg id =
  match
    reg |> Array.to_list
    |> List.mapi (fun i r -> (i, r))
    |> List.find_opt (fun (i, r) -> i = int_of_id_t id)
  with
  | Some (i, v) -> (i, v)
  | None -> assert false

let find_reds ~reg ~args =
  args |> List.filter (fun a -> a |> int_of_id_t |> Array.get reg |> is_red)

let rec connect (id_t, typ) instr body =
  let rec go id_t body = function
    | Let (a, e, t) -> Let (a, e, go id_t t body)
    | Ans e -> Let ((id_t, typ), e, body)
  in
  go id_t instr body

let rec zip x y =
  match (x, y) with
  | [], [] -> []
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: zip t1 t2
  | _ -> failwith "the lengths of x and y are not equeal"

module Guard = Jit_guard

let pc_header = ref 0

let is_re_merge_point = ref false

let (|%|) e (x, y, t1, t2) = match e with
  | IfEq _ | SIfEq _ -> IfEq (x, y, t1, t2)
  | IfGE _ | SIfGE _ -> IfGE (x, y, t1, t2)
  | IfLE _ | SIfLE _ -> IfLE (x, y, t1, t2)
  | _ -> assert false

let rec tj (p : prog) (reg : value array) (mem : value array) (tj_env : Jit_env.env) =
  function
  | Ans exp -> tj_exp p reg mem tj_env exp
  | Let ((dest, typ), CallDir (Id.L "min_caml_can_enter_jit", args, fargs), body) ->
    let { merge_pc } = tj_env in
    let pc = List.last args |> int_of_id_t |> Array.get reg |> value_of in
    Log.debug @@ Printf.sprintf "merge_pc: %d, pc: %d" merge_pc pc;
    if pc = merge_pc then
      let reds = List.nth args 0 :: List.nth args 1 :: [] in
      Log.debug ("can_enter_jit: pc " ^ string_of_int pc) ;
      let {index_pc; merge_pc; trace_name} = tj_env in
      Ans (CallDir (Id.L trace_name, reds, []))
    else
      tj p reg mem tj_env body
  | Let ((dest, typ), CallDir (Id.L "min_caml_jit_merge_point", args, fargs), body) ->
    let pc = List.hd args |> int_of_id_t |> Array.get reg |> value_of in
    Log.debug (Printf.sprintf "jit_merge_point: pc %d" pc) ;
    tj p reg mem tj_env body
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) ->
     let callee =
       Fundef.find p id_l
       |> Inlining.inline_fundef reg args
       |> tj p reg mem tj_env
     in
     Asm.concat callee (dest, typ) (tj p reg mem tj_env body)
  | Let ((dest, typ), exp, body) ->
    begin match exp with
    | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) | IfGE (id_t, id_or_imm, t1, t2)
    | SIfEq (id_t, id_or_imm, t1, t2) | SIfLE (id_t, id_or_imm, t1, t2) | SIfGE (id_t, id_or_imm, t1, t2) ->
       Asm.concat (tj_if p reg mem tj_env exp) (dest, typ) (tj p reg mem tj_env body)
    | St (id_t1, id_t2, id_or_imm, x) ->
      let srcv = reg.(int_of_id_t id_t1) in
      let destv = reg.(int_of_id_t id_t2) in
      let offsetv = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
      let body' = tj p reg mem tj_env body in
      begin match (srcv, destv) with
      | Green n1, Red n2 | LightGreen n1, Red n2 -> (
        reg.(int_of_id_t dest) <- Green 0 ;
        mem.(n1 + (n2 * x)) <- Green (int_of_id_t id_t1 |> Array.get reg |> value_of);
        match offsetv with
        | Green n | LightGreen n ->
           let id' = Id.gentmp Type.Int in
           Let
             ( (id_t1, Type.Int)
             , Set n1
             , Let
                 ( (id', Type.Int)
                 , Set n
                 , Let ((dest, typ), St (id_t1, id_t2, C n, x), body') ) )
        | Red n ->
           Let
             ( (id_t1, Type.Int)
             , Set n1
             , Let ((dest, typ), St (id_t1, id_t2, id_or_imm, x), body') ) )
      | _ -> optimize_exp p reg mem tj_env (dest, typ) body exp
      end
    | _ -> optimize_exp p reg mem tj_env (dest, typ) body exp
    end

and optimize_exp p reg mem tj_env (dest, typ) body exp =
  match Jit_optimizer.run p exp reg mem with
  | Specialized v ->
    reg.(int_of_id_t dest) <- v ;
    tj p reg mem tj_env body
  | Not_specialized (e, v) ->
    reg.(int_of_id_t dest) <- v ;
    Let ((dest, typ), e, tj p reg mem tj_env body)

and tj_exp (p : prog) (reg : value array) (mem : value array) (tj_env : Jit_env.env) =
  function
  | CallDir (id_l, args, fargs) ->
     Log.debug (Printf.sprintf "CallDir (%s)" (Id.string_of_id_l id_l)) ;
     let fundef = Fundef.find p id_l in
     let pc = args |> find_pc tj_env |> Array.get reg in
     let reds = args |> List.filter (fun a -> is_red reg.(int_of_id_t a)) in
     let {merge_pc; trace_name} = tj_env in
     if value_of pc = merge_pc && let (Id.L x) = id_l in String.contains x "interp"
     then Ans (CallDir (Id.L trace_name, reds, []))
     else Inlining.inline_fundef reg args fundef |> tj p reg mem tj_env
  | IfEq _ | IfLE _ | IfGE _ | SIfEq _ | SIfLE _ | SIfGE _ as exp ->
     tj_if p reg mem tj_env exp
  | exp ->
     match Jit_optimizer.run p exp reg mem with
     | Specialized v -> Ans (Set (value_of v))
     | Not_specialized (e, v) -> Ans e

and tj_if (p : prog) (reg : value array) (mem : value array) (tj_env : Jit_env.env) =
  let trace = tj p reg mem tj_env in
  let guard = Guard.create_tj reg tj_env in
  function
  | IfLE (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
    Log.debug @@ Printf.sprintf "IfLE (%s, %s) ==> %d %d"
      id_t (string_of_id_or_imm id_or_imm) (value_of r1) (value_of r2);
    begin
      match r1, r2 with
      | Green n1, Green n2 ->
        if n1 <= n2 then trace t1
        else trace t2
      | Red n1, Green n2 ->
        if n1 <= n2 then
          Ans (IfLE (id_t, C (n2), trace t1, guard t2))
        else
          Ans (IfLE (id_t, C (n2), guard t1, trace t2))
      | Green n1, Red n2 ->
        let id_t2 = match id_or_imm with
            V (id) -> id
          | C _ -> failwith "un matched pattern."
        in
        if n1 <= n2 then
          Ans (IfGE (id_t2, C (n1), trace t1, guard t2))
        else
          Ans (IfGE (id_t2, C (n1), guard t1, trace t2))
      | Red n1, Red n2 ->
        if n1 <= n2 then
          Ans (IfLE (id_t, id_or_imm, trace t1, guard t2))
        else
          Ans (IfLE (id_t, id_or_imm, guard t1, trace t2))
    end
  | SIfLE (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
    Log.debug @@ Printf.sprintf "IfLE (%s, %s) ==> %d %d"
      id_t (string_of_id_or_imm id_or_imm) (value_of r1) (value_of r2);
    let guard = trace in
    begin
      match r1, r2 with
      | Green n1, Green n2 ->
        if n1 <= n2 then trace t1
        else trace t2
      | Red n1, Green n2 ->
        if n1 <= n2 then
          Ans (IfLE (id_t, C (n2), trace t1, guard t2))
        else
          Ans (IfLE (id_t, C (n2), guard t1, trace t2))
      | Green n1, Red n2 ->
        let id_t2 = match id_or_imm with
            V (id) -> id
          | C _ -> failwith "un matched pattern."
        in
        if n1 <= n2 then
          Ans (IfGE (id_t2, C (n1), trace t1, guard t2))
        else
          Ans (IfGE (id_t2, C (n1), guard t1, trace t2))
      | Red n1, Red n2 ->
        if n1 <= n2 then
          Ans (IfLE (id_t, id_or_imm, trace t1, guard t2))
        else
          Ans (IfLE (id_t, id_or_imm, guard t1, trace t2))
    end
  | IfEq (id_t, id_or_imm, t1, t2) | SIfEq (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
    Log.debug @@ Printf.sprintf "IfEq (%s, %s) ==> %d %d"
      id_t (string_of_id_or_imm id_or_imm) (value_of r1) (value_of r2);
    begin
      match r1, r2 with
      | Green n1, Green n2 ->
        if n1 = n2 then
          tj p reg mem tj_env t1
        else
          tj p reg mem tj_env t2
      | Red n1, Green n2 ->
        if n1 = n2 then
          Ans (IfEq (id_t, C (n2), trace t1, guard t2))
        else
          Ans (IfEq (id_t, C (n2), guard t1, trace t2))
      | Green n1, Red n2 ->
        let id_t2 = match id_or_imm with
            V (id) -> id
          | C _ -> failwith "un matched pattern."
        in
        if n1 = n2 then
          Ans (IfEq (id_t2, C (n1), trace t1, guard t2))
        else
          Ans (IfEq (id_t, C (n1), guard t1, trace t2))
      | Red n1, Red n2 ->
        if n1 = n2 then
          Ans (IfEq (id_t, id_or_imm, trace t1, guard t2))
        else
          Ans (IfEq (id_t, id_or_imm, guard t1, trace t2))
    end
  | IfGE (id_t, id_or_imm, t1, t2) | SIfGE (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
    Log.debug @@ Printf.sprintf "IfEq (%s, %s) ==> %d %d"
      id_t (string_of_id_or_imm id_or_imm) (value_of r1) (value_of r2);
    begin
      match r1, r2 with
      | Green n1, Green n2 ->
        if n1 >= n2 then
          tj p reg mem tj_env t1
        else
          tj p reg mem tj_env t2
      | Red n1, Green n2 ->
        if n1 >= n2 then
          Ans (IfGE (id_t, C (n2), trace t1, guard t2))
        else
          Ans (IfGE (id_t, C (n2), guard t1, trace t2))
      | Green n1, Red n2 ->
        let id_t2 = match id_or_imm with
            V (id) -> id
          | C _ -> failwith "un matched pattern."
        in
        if n1 >= n2 then
          Ans (IfLE (id_t2, C (n1), trace t1, guard t2))
        else
          Ans (IfLE (id_t2, C (n1), guard t1, trace t2))
      | Red n1, Red n2 ->
        if n1 >= n2 then
          Ans (IfGE (id_t, id_or_imm, trace t1, guard t2))
        else
          Ans (IfGE (id_t, id_or_imm, guard t1, trace t2))
    end
  | e -> (
    match Jit_optimizer.run p e reg mem with
    | Specialized v -> Ans (Set (value_of v))
    | Not_specialized (e, v) -> Ans e )

let run p reg mem ({trace_name; red_names; index_pc; merge_pc;} as env) =
  Renaming.counter := 0;
  let (Prog (tbl, fundefs, m)) = p in
  let {body= ibody; args= iargs} = Fundef.find_fuzzy p "interp" in
  let trace = ibody |> tj p reg mem env in
  { name= Id.L trace_name
  ; args= iargs |> List.filter (fun arg -> List.mem (String.get_name arg) red_names)
  ; fargs= []
  ; body= trace
  ; ret= Type.Int}
