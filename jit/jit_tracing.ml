open Std
open MinCaml
open Asm
open Jit_env
open Jit_util
open Jit_prof
open Printf
module JO = Jit_optimizer
module Util = Jit_tracer_util

let sp = sprintf
let other_deps : string list ref = ref []
let re_entry = ref false
let interp_fundef p = Fundef.find_fuzzy p "interp"

let rec tj
    p
    reg
    mem
    ({ trace_name; red_names; index_pc; merge_pc; bytecode } as env)
    t
  =
  match t with
  | Ans (CallDir (id_l, args, fargs)) ->
    let pc = Util.value_of_id_t reg (List.nth args index_pc) in
    (match Method_prof.find_opt pc, Trace_prof.find_opt pc with
    | None, None ->
      let { name; args = argst; fargs; body; ret } = interp_fundef p in
      { name; args = argst; fargs; body; ret }
      |> Inlining.inline_fundef reg args
      |> tj p reg mem env
    | Some tname, None ->
      other_deps := !other_deps @ [ tname ];
      Ans (CallDir (Id.L tname, Util.filter red_names args, []))
    | None, Some tname ->
      other_deps := !other_deps @ [ tname ];
      Ans (CallDir (Id.L tname, Util.filter red_names args, []))
    | Some tname, _ ->
      other_deps := !other_deps @ [ tname ];
      Ans (CallDir (Id.L tname, Util.filter red_names args, []))
    )
  | Ans exp ->
    (match exp with
    | IfEq _ | IfLE _ | IfGE _ | SIfEq _ | SIfLE _ | SIfGE _ ->
      tj_if p reg mem env exp
    | _ ->
      (match JO.run p exp reg mem with
      | Specialized v -> Ans (Set (value_of v))
      | Not_specialized (e, v) -> Ans e))
  | Let ((dest, typ), e, body) -> tj_exp p reg mem env (dest, typ) body e

and tj_exp
    p
    reg
    mem
    ({ trace_name; red_names; index_pc; merge_pc; bytecode } as env)
    (dest, typ)
    body
  = function
  | (IfEq _ | IfLE _ | IfGE _ | SIfEq _ | SIfLE _ | SIfGE _) as e ->
    Asm.concat (tj_if p reg mem env e) (dest, typ) (tj p reg mem env body)
  | CallDir (Id.L "min_caml_jit_merge_point", args, fargs) ->
    let id_pc = List.hd args in
    let pc = value_of @@ reg.(int_of_id_t id_pc) in
    Log.debug @@ sprintf "jit_merge_point: %d" pc;
    if pc = merge_pc
    then
      if !re_entry
      then Ans (CallDir (Id.L trace_name, Util.filter ~reds:red_names args, []))
      else (
        re_entry := true;
        tj p reg mem env body)
    else tj p reg mem env body
  | CallDir (Id.L "min_caml_method_entry", args, fargs) -> tj p reg mem env body
  | CallDir (Id.L "min_caml_guard_promote", args, fargs) ->
    tj p reg mem env body
  | CallDir (Id.L "min_caml_can_enter_jit", args, fargs) ->
    let pc = value_of @@ reg.(int_of_id_t @@ List.nth args index_pc) in
    Log.debug @@ sprintf "can_enter_jit: %d" pc;
    if pc = merge_pc
    then Ans (CallDir (Id.L trace_name, Util.filter red_names args, []))
    else tj p reg mem env body
  | CallDir (Id.L "min_caml_mj_call", args', fargs') ->
    let pc = Util.value_of_id_t reg (List.nth args' index_pc) in
    (match Method_prof.find_opt pc with
    | Some tname ->
      other_deps := !other_deps @ [ tname ];
      Let
        ( (dest, typ)
        , CallDir (Id.L tname, Util.filter ~reds:red_names args', fargs')
        , tj p reg mem env body )
    | None ->
      Util.restore_greens reg args' (fun () ->
          Let
            ( (dest, typ)
            , CallDir (Id.L "interp_no_hints", args', fargs')
            , tj p reg mem env body )))
  | CallDir (Id.L x, args, fargs) when String.starts_with x "cast_" ->
    Let ((dest, typ), CallDir (Id.L x, args, fargs), tj p reg mem env body)
  | CallDir (Id.L x, args, fargs) when String.(starts_with x "frame_reset") ->
    Util.restore_greens reg args (fun () ->
        Let
          ( (dest, typ)
          , CallDir (Id.L x, args, fargs)
          , tj p reg mem env body ))
  | CallDir (Id.L x, args, fargs) when String.(starts_with x "min_caml") ->
    (* foreign functions *)
    Util.restore_greens reg args (fun () ->
        Let
          ( (dest, typ)
          , CallDir (Id.L x, Util.filter ~reds:red_names args, fargs)
          , tj p reg mem env body ))
  | CallDir (Id.L x, argsr, fargsr) ->
    (* inline function call *)
    let inlined_fun =
      Fundef.find_fuzzy p x
      |> Inlining.inline_fundef reg argsr
      |> tj p reg mem env
    in
    Asm.concat inlined_fun (dest, typ) (tj p reg mem env body)
  | St (id_t1, id_t2, id_or_imm, x) as exp ->
    let srcv = reg.(int_of_id_t id_t1) in
    let destv = reg.(int_of_id_t id_t2) in
    let offsetv =
      match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n
    in
    (match srcv, destv with
    | Green n1, Red n2 ->
      reg.(int_of_id_t dest) <- Green 0;
      mem.(n1 + (n2 * x))
        <- Green (int_of_id_t id_t1 |> Array.get reg |> value_of);
      (match offsetv with
      | Green n ->
        let id' = Id.gentmp Type.Int in
        let body' = tj p reg mem env body in
        Let
          ( (id_t1, Type.Int)
          , Set n1
          , Let
              ( (id', Type.Int)
              , Set n
              , Let ((dest, typ), St (id_t1, id_t2, C n, x), body') ) )
      | Red n ->
        let body' = tj p reg mem env body in
        Let
          ( (id_t1, Type.Int)
          , Set n1
          , Let ((dest, typ), St (id_t1, id_t2, id_or_imm, x), body') ))
    | _ -> body |> optimize_exp p reg mem env (dest, typ) exp)
  | exp -> body |> optimize_exp p reg mem env (dest, typ) exp

and optimize_exp p reg mem env (dest, typ) exp body =
  match JO.run p exp reg mem with
  | Specialized v ->
    reg.(int_of_id_t dest) <- v;
    tj p reg mem env body
  | Not_specialized (e, v) ->
    reg.(int_of_id_t dest) <- v;
    Let ((dest, typ), e, tj p reg mem env body)

and tj_if p reg mem env exp =
  let trace = tj p reg mem env in
  let guard = Jit_guard.TJ.create reg env.trace_name in
  match exp with
  | IfLE (id_t, id_or_imm, t1, t2) | SIfLE (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 =
      match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n
    in
    (match r1, r2 with
    | Green n1, Green n2 -> if n1 <= n2 then trace t1 else trace t2
    | Red n1, Green n2 ->
      if n1 <= n2
      then Ans (IfLE (id_t, C n2, trace t1, guard t2))
      else Ans (IfLE (id_t, C n2, guard t1, trace t2))
    | Green n1, Red n2 ->
      let id_t2 =
        match id_or_imm with
        | V id -> id
        | C _ -> failwith "un matched pattern."
      in
      if n1 <= n2
      then Ans (IfGE (id_t2, C n1, trace t2, guard t1))
      else Ans (IfGE (id_t2, C n1, guard t2, trace t1))
    | Red n1, Red n2 ->
      if n1 <= n2
      then Ans (IfLE (id_t, id_or_imm, trace t1, guard t2))
      else Ans (IfLE (id_t, id_or_imm, guard t1, trace t2)))
  | IfEq (id_t, id_or_imm, t1, t2) | SIfEq (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 =
      match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n
    in
    if String.get_name id_t = "mode"
    then (
      Log.debug
      @@ Printf.sprintf
           "IfEq mode checking (%s, %s) ==> %d %d"
           id_t
           (string_of_id_or_imm id_or_imm)
           (value_of r1)
           (value_of r2);
      Ans (IfEq (id_t, C 200, trace t1, guard t2)))
    else (
      match r1, r2 with
      | Green n1, Green n2 ->
        if n1 = n2 then tj p reg mem env t1 else tj p reg mem env t2
      | Red n1, Green n2 ->
        if n1 = n2
        then Ans (IfEq (id_t, C n2, trace t1, guard t2))
        else Ans (IfEq (id_t, C n2, guard t1, trace t2))
      | Green n1, Red n2 ->
        let id_t2 =
          match id_or_imm with
          | V id -> id
          | C _ -> failwith "un matched pattern."
        in
        if n1 = n2
        then Ans (IfEq (id_t2, C n1, trace t1, guard t2))
        else Ans (IfEq (id_t2, C n1, guard t1, trace t2))
      | Red n1, Red n2 ->
        if n1 = n2
        then Ans (IfEq (id_t, id_or_imm, trace t1, guard t2))
        else Ans (IfEq (id_t, id_or_imm, guard t1, trace t2)))
  | IfGE (id_t, id_or_imm, t1, t2) | SIfGE (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 =
      match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n
    in
    (match r1, r2 with
    | Green n1, Green n2 ->
      if n1 >= n2 then tj p reg mem env t1 else tj p reg mem env t2
    | Red n1, Green n2 ->
      if n1 >= n2
      then Ans (IfGE (id_t, C n2, trace t1, guard t2))
      else Ans (IfGE (id_t, C n2, guard t1, trace t2))
    | Green n1, Red n2 ->
      let id_t2 =
        match id_or_imm with
        | V id -> id
        | C _ -> failwith "un matched pattern."
      in
      if n1 >= n2
      then Ans (IfLE (id_t2, C n1, trace t1, guard t2))
      else Ans (IfLE (id_t2, C n1, guard t1, trace t2))
    | Red n1, Red n2 ->
      if n1 >= n2
      then Ans (IfGE (id_t, id_or_imm, trace t1, guard t2))
      else Ans (IfGE (id_t, id_or_imm, guard t1, trace t2)))
  | _ -> failwith "tj_if: unmatched pattern."
;;

let run p reg mem ({ trace_name; red_names; merge_pc } as env) =
  Renaming.counter := !Id.counter + 1;
  other_deps := [];
  re_entry := false;
  Log.debug @@ Printf.sprintf "staring trace (merge_pc: %d)" merge_pc;
  let fenv name = Fundef.find_fuzzy p name in
  let (Prog (tbl, _, fundefs, m)) = p in
  let { body = body'; args = args' } = fenv "interp" in
  let trace = body' |> tj p reg mem env in
  `Result
    ( { name = Id.L trace_name
      ; fargs = []
      ; body = trace
      ; ret = Type.Int
      ; args = Util.filter ~reds:red_names args'
      }
    , Option.some !other_deps )
;;
