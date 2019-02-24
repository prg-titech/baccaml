open Utils
open Std
open MinCaml
open Asm
open Inlining
open Renaming
open Operands
open Jit_util

type tj_env =
  { trace_name: string
  ; red_args: string list
  ; index_pc: int
  ; merge_pc: int
  ; bytecode_ptr: int
  ; stack_ptr: int }

let rec unique list =
  let rec go l s =
    match l with
    | [] -> s
    | first :: rest ->
        if List.exists (fun e -> e = first) s then go rest s else go rest (s @ [first])
  in
  go list []

let find_pc {index_pc} args =
  match List.nth_opt args index_pc with
  | Some s -> int_of_id_t s
  | None -> failwith "find_pc is failed"

let find_fundef name prog =
  let (Prog (_, fundefs, _)) = prog in
  match fundefs |> List.find_opt (fun fundef -> fundef.name = name) with
  | Some body -> body
  | None ->
      let (Id.L x) = name in
      failwith @@ Printf.sprintf "find_fundef is failed: %s" x

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

module Guard = struct
  let rec add_guard_label reg path tj_env = function
    | Ans (CallDir (Id.L x, args, fargs)) -> (
        let {index_pc; merge_pc; trace_name} = tj_env in
        let pc =
          match List.nth_opt args index_pc with
          | Some id -> reg.(int_of_id_t id) |> value_of
          | None ->
              failwith (Printf.sprintf "specified index_pc is invalid: %d" index_pc)
        in
        match path with
        | `True when pc = merge_pc -> Ans (CallDir (Id.L trace_name, args, fargs))
        | `True -> Ans (CallDir (Id.L (x), args, fargs))
        | `False -> Ans (CallDir (Id.L "min_caml_mid_layer", args, fargs)) )
    | Ans exp -> Ans exp
    | Let ((id, typ), exp, body) ->
        Let ((id, typ), exp, add_guard_label reg path tj_env body)

  let create_guard reg tj_env ?wlist:(ws = []) cont =
    let free_vars = unique (fv cont) in
    let ignored x ys =
      ys |> List.exists (fun y -> String.split_on_char '.' x |> List.hd = y)
    in
    let rec restore cont = function
      | [] -> cont
      | hd :: tl when not (ignored hd ws) -> (
        match reg.(int_of_id_t hd) with
        | Green n when (String.contains hd "bytecode") ->
           let { bytecode_ptr } = tj_env in
           Let ((hd, Type.Int), Set bytecode_ptr, restore cont tl)
        | Green n -> Let ((hd, Type.Int), Set n, restore cont tl)
        | _ -> restore cont tl )
      | hd :: tl -> restore cont tl
    in
    restore cont free_vars
end

let pc_header = ref 0

let is_re_merge_point = ref false

let rec tj (p : prog) (reg : value array) (mem : value array) (tj_env : tj_env) =
  function
  | Ans exp -> tj_exp p reg mem tj_env exp
  | Let ((dest, typ), CallDir (Id.L "min_caml_can_enter_jit", args, fargs), body) ->
      let pc = List.hd args |> int_of_id_t |> Array.get reg |> value_of in
      Log.debug ("can_enter_jit: pc " ^ string_of_int pc) ;
      let {index_pc; merge_pc; trace_name} = tj_env in
      Ans (CallDir (Id.L trace_name, List.tl args, []))
  | Let ((dest, typ), CallDir (Id.L "min_caml_jit_merge_point", args, fargs), body) ->
      let {index_pc; merge_pc; trace_name} = tj_env in
      let pc = List.hd args |> int_of_id_t |> Array.get reg |> value_of in
      let reds = List.tl args in
      Log.debug (Printf.sprintf "jit_merge_point: pc %d" pc) ;
      if not !is_re_merge_point then (
        pc_header := pc ;
        is_re_merge_point := true ;
        tj p reg mem tj_env body )
      else if !is_re_merge_point && !pc_header = pc then
        Ans (CallDir (Id.L trace_name, reds, []))
      else tj p reg mem tj_env body
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) ->
      let rec f lst acc =
        match lst with
        | [] -> acc
        | (l, r) :: tl -> f tl (Let ((l, Type.Int), Mov r, acc))
      in
      let fundef = find_fundef id_l p in
      let t = Inlining.inline_fundef reg args fundef |> tj p reg mem tj_env in
      connect (dest, typ) (tj p reg mem tj_env body) (f (zip fundef.args args) t)
  | Let ((dest, typ), exp, body) -> (
    match exp with
    | IfEq (id_t, id_or_imm, t1, t2)
     |IfLE (id_t, id_or_imm, t1, t2)
     |IfGE (id_t, id_or_imm, t1, t2) ->
        connect (dest, typ) (tj_if p reg mem tj_env exp) (tj p reg mem tj_env body)
    | Ld (id_t, id_or_imm, x) -> (
        let r1 = int_of_id_t id_t |> Array.get reg in
        let r2 =
          match id_or_imm with
          | V id -> int_of_id_t id_t |> Array.get reg
          | C n -> Green n
        in
        match (r1, r2) with
        | Green n1, Red n2 | LightGreen n1, Red n2 ->
            let n = mem.(n1 + (n2 * x)) in
            reg.(int_of_id_t id_t) <- n ;
            Let ((dest, typ), Ld (id_t, id_or_imm, x), tj p reg mem tj_env body)
        | _ -> optimize_exp p reg mem tj_env (dest, typ) body exp )
    | St (id_t1, id_t2, id_or_imm, x) -> (
        let srcv = reg.(int_of_id_t id_t1) in
        let destv = reg.(int_of_id_t id_t2) in
        let offsetv =
          match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n
        in
        let body' = tj p reg mem tj_env body in
        match (srcv, destv) with
        | Green n1, Red n2 | LightGreen n1, Red n2 -> (
            reg.(int_of_id_t dest) <- Green 0 ;
            mem.(n1 + (n2 * x)) <- Green (int_of_id_t id_t1 |> Array.get reg |> value_of) ;
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
        | _ -> optimize_exp p reg mem tj_env (dest, typ) body exp )
    | _ -> optimize_exp p reg mem tj_env (dest, typ) body exp )

and optimize_exp p reg mem tj_env (dest, typ) body exp =
  match Jit_optimizer.run p exp reg mem with
  | Specialized v ->
      reg.(int_of_id_t dest) <- v ;
      tj p reg mem tj_env body
  | Not_specialized (e, v) ->
      reg.(int_of_id_t dest) <- v ;
      Let ((dest, typ), e, tj p reg mem tj_env body)

and tj_exp (p : prog) (reg : value array) (mem : value array) (tj_env : tj_env) =
  function
  | CallDir (id_l, args, fargs) ->
      Log.debug (Printf.sprintf "CallDir (%s)" (string_of_id_l id_l)) ;
      let fundef = find_fundef id_l p in
      let pc = args |> find_pc tj_env |> Array.get reg in
      let reds = args |> List.filter (fun a -> is_red reg.(int_of_id_t a)) in
      let {merge_pc; trace_name} = tj_env in
      if
        value_of pc = merge_pc
        &&
        let (Id.L x) = id_l in
        contains x "interp"
      then Ans (CallDir (Id.L trace_name, reds, []))
      else Inlining.inline_fundef reg args fundef |> tj p reg mem tj_env
  | IfEq (_, _, Ans (CallDir (id_l, _, _)), t2)
    when let (Id.L x) = id_l in
         contains x "trace" ->
      tj p reg mem tj_env t2
  | ( IfEq (id_t, id_or_imm, t1, t2)
    | IfLE (id_t, id_or_imm, t1, t2)
    | IfGE (id_t, id_or_imm, t1, t2) ) as exp ->
      tj_if p reg mem tj_env exp
  | exp -> (
    match Jit_optimizer.run p exp reg mem with
    | Specialized v -> Ans (Set (value_of v))
    | Not_specialized (e, v) -> Ans e )

and tj_if (p : prog) (reg : value array) (mem : value array) (tj_env : tj_env) = function
  | ( IfEq (id_t, id_or_imm, t1, t2)
    | IfLE (id_t, id_or_imm, t1, t2)
    | IfGE (id_t, id_or_imm, t1, t2) ) as exp -> (
      Log.debug
        (let if_rep =
           match exp with
           | IfEq _ -> "IfEq"
           | IfLE _ -> "IfLE"
           | IfGE _ -> "IfGE"
           | _ -> failwith "If expression should be come here."
         in
         Printf.sprintf "%s (%s, %s)" if_rep id_t (string_of_id_or_imm id_or_imm)) ;
      let r1 = reg.(int_of_id_t id_t) in
      let r2 = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
      match (r1, r2) with
      | Green n1, Green n2
       |LightGreen n1, Green n2
       |Green n1, LightGreen n2
       |LightGreen n1, LightGreen n2 ->
          if exp |*| (n1, n2) then tj p reg mem tj_env t1 else tj p reg mem tj_env t2
      | Red n1, Green n2 | Red n1, LightGreen n2 ->
          if exp |*| (n1, n2) then
            Ans
              ( exp
              |%| ( id_t
                  , C n2
                  , tj p reg mem tj_env t1
                  , Guard.create_guard reg tj_env t2
                    |> tj_guard_over p reg mem `False tj_env ) )
          else
            Ans
              ( exp
              |%| ( id_t
                  , C n2
                  , Guard.create_guard reg tj_env t1
                    |> tj_guard_over p reg mem `False tj_env
                  , tj p reg mem tj_env t2 ) )
      | Green n1, Red n2 | LightGreen n1, Red n2 ->
          let id_r2 =
            match id_or_imm with
            | V id -> id
            | C n -> failwith "id_or_imm should be string"
          in
          if exp |*| (n1, n2) then
            Ans
              ( exp
              |%| ( id_r2
                  , C n1
                  , tj p reg mem tj_env t1
                  , Guard.create_guard reg tj_env t2
                    |> tj_guard_over p reg mem `False tj_env ) )
          else
            Ans
              ( exp
              |%| ( id_r2
                  , C n1
                  , Guard.create_guard reg tj_env t1
                    |> tj_guard_over p reg mem `False tj_env
                  , tj p reg mem tj_env t2 ) )
      | Red n1, Red n2 ->
          if exp |*| (n1, n2) then
            Ans
              ( exp
              |%| ( id_t
                  , id_or_imm
                  , tj p reg mem tj_env t1
                  , Guard.create_guard reg tj_env t2
                    |> tj_guard_over p reg mem `False tj_env ) )
          else
            Ans
              ( exp
              |%| ( id_t
                  , id_or_imm
                  , Guard.create_guard reg tj_env t1
                    |> tj_guard_over p reg mem `False tj_env
                  , tj p reg mem tj_env t2 ) ) )
  | e -> (
    match Jit_optimizer.run p e reg mem with
    | Specialized v -> Ans (Set (value_of v))
    | Not_specialized (e, v) -> Ans e )

and tj_guard_over p reg mem path tj_env = function
  | Ans (CallDir (Id.L x, args, fargs)) -> (
      let fundef = find_fundef (Id.L x) p in
      let {index_pc; merge_pc; trace_name} = tj_env in
      let pc =
        match List.nth_opt args index_pc with
        | Some id -> reg.(int_of_id_t id) |> value_of
        | None -> failwith (Printf.sprintf "specified index_pc is invalid: %d" index_pc)
      in
      match path with
      | `True when pc = merge_pc -> Ans (CallDir (Id.L trace_name, args, fargs))
      | `True -> Inlining.inline_fundef reg args fundef |> tj p reg mem tj_env
      | `False -> Ans (CallDir (Id.L (x), args, fargs)) )
  | Ans exp -> Ans exp
  | Let ((id, typ), exp, body) ->
      Let ((id, typ), exp, tj_guard_over p reg mem path tj_env body)

(* mem name reds index_pc merge_pc *)
let run_while p reg mem
    ({trace_name; red_args; index_pc; merge_pc; bytecode_ptr; stack_ptr} as env) =
  let (Prog (tbl, fundefs, m)) = p in
  let {body= ibody} = find_fundef' p "interp" in
  let trace = ibody |> tj p reg mem env in
  {name= Id.L trace_name; args= red_args; fargs= []; body= trace; ret= Type.Int}

let run p reg mem tj_env = run_while p reg mem tj_env
