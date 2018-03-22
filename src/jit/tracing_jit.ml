open Asm
open Core
open Guard
open Jit_config
open Jit_util
open Renaming
open Inlining

exception Not_supported of string

let find_pc args jit_args =
  match List.nth args (jit_args.loop_pc_place) with
  | Some (s) -> int_of_id_t s
  | None -> failwith "find_pc is failed"

let rec add_cont_proc id_t instr body =
  let rec go id_t instr body = match instr with
    | Let (a, e, t) ->
      Let (a, e, go id_t t body)
    | Ans e ->
      Let ((id_t, Type.Int), e, body)
  in go id_t instr body

let rec tracing_jit : prog -> t -> reg -> mem -> jit_args -> t =
  fun p instr reg mem jit_args -> match instr with
  | Ans (exp) ->
    tracing_jit_ans p exp reg mem jit_args
  | Let ((dest, typ), CallDir (id_l, argsr, argst), body) ->
    let fundef = (find_fundef p id_l) in
    let t = tracing_jit p (inline_calldir_exp argsr fundef reg) reg mem jit_args in
    add_cont_proc dest t (tracing_jit p body reg mem jit_args)
  | Let ((dest, typ), instr, body) ->
    begin match Optimizer.optimize_exp p instr reg mem with
     | Specialized v ->
       reg.(int_of_id_t dest) <- v;
       tracing_jit p body reg mem jit_args
     | Not_specialized (e, v) ->
       reg.(int_of_id_t dest) <- v;
       Let ((dest, typ), e, tracing_jit p body reg mem jit_args)
    end

and tracing_jit_ans p e reg mem jit_args = match e with
  | CallDir (id_l, args, _) ->
    let fundef = find_fundef p id_l in
    let pc = value_of reg.(find_pc args jit_args) in
    begin match (pc = (jit_args.loop_header)) with
      | true ->
        let reds = args |> List.filter ~f:(fun a -> is_red reg.(int_of_id_t a)) in
        Ans (CallDir (Id.L (jit_args.trace_name), reds, []))
      | false ->
        tracing_jit p (inline_calldir_exp args fundef reg) reg mem jit_args
    end
  | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) | IfGE (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> reg.(int_of_id_t id)
      | C (n) -> Green (n)
    in
    (match r1, r2 with
     | Green (n1), Green (n2) ->
       tracing_jit p (select_branch e n1 n2 t1 t2) reg mem jit_args
     | Green (n1), Red (n2) ->
       let id_r2 = match id_or_imm with
           V (id) -> id
         | C _ -> failwith "V (id) should be come here."
       in
       (match e with
        | IfEq _ ->
          if n1 = n2 then
            Ans (IfEq (id_r2, C (n1), tracing_jit p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfEq (id_r2, C (n1), restore_green reg t1, tracing_jit p t2 reg mem jit_args))
        | IfLE _ ->
          if n1 <= n2 then
            Ans (IfLE (id_r2, C (n1), tracing_jit p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfLE (id_r2, C (n1), restore_green reg t1, tracing_jit p t2 reg mem jit_args))
        | IfGE _ ->
          if n1 >= n2 then
            Ans (IfGE (id_r2, C (n1), tracing_jit p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfGE (id_r2, C (n1), restore_green reg t1, tracing_jit p t2 reg mem jit_args))
        | _ ->
          failwith "Not supported"
       )
     | Red (n1), Green (n2) ->
       (match e with
        | IfEq _ ->
          if n1 = n2 then
            Ans (IfEq (id_t, C (n2), tracing_jit p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfEq (id_t, C (n2), restore_green reg t1, tracing_jit p t2 reg mem jit_args))
        | IfLE _ ->
          if n1 <= n2 then
            Ans (IfLE (id_t, C (n2), tracing_jit p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfLE (id_t, C (n2), restore_green reg t1, tracing_jit p t2 reg mem jit_args))
        | IfGE _ ->
          if n1 >= n2 then
            Ans (IfGE (id_t, C (n2), tracing_jit p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfGE (id_t, C (n2), restore_green reg t1, tracing_jit p t2 reg mem jit_args))
        | _ ->
          failwith "Not supported"
       )
     | Red (n1), Red (n2) ->
       (match e with
        | IfEq _ ->
          if n1 = n2 then
            Ans (IfEq (id_t, id_or_imm, tracing_jit p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfEq (id_t, id_or_imm, restore_green reg t1, tracing_jit p t2 reg mem jit_args))
        | IfLE _ ->
          if n1 <= n2 then
            Ans (IfLE (id_t, id_or_imm, tracing_jit p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfLE (id_t, id_or_imm, restore_green reg t1, tracing_jit p t2 reg mem jit_args))
        | IfGE _ ->
          if n1 >= n2 then
            Ans (IfGE (id_t, id_or_imm, tracing_jit p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfGE (id_t, id_or_imm, restore_green reg t1, tracing_jit p t2 reg mem jit_args))
        | _ ->
          failwith "Not supported"
       ))
  | _ ->
    begin
      match Optimizer.optimize_exp p e reg mem with
      | Specialized (v) ->
        Ans (Nop)
      | Not_specialized (e, v) ->
        Ans (e)
    end

let exec_tracing_jit p t reg mem jit_args =
  let res =
    tracing_jit p t reg mem jit_args
  in
  { name = Id.L (jit_args.trace_name)
  ; args = jit_args.reds
  ; fargs = []
  ; body = res
  ; ret = Type.Int
  }
