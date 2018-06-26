open Mincaml
open Util
open Asm
open Core
open Inlining
open Jit_config
open Jit_util
open Renaming


let rec find_loop p t reg mem jargs =
  match t with
  | Ans (IfEq (_, _, Ans CallDir (Id.L ("min_caml_trace_entry"), _, _), body)) ->
    find_loop_start p body reg mem jargs
  | _ ->
    find_loop_start p t reg mem jargs

and find_loop_start p t reg mem jargs =
  match t with
  | Let (x, CallDir (Id.L ("min_caml_loop_start"), rargs, fargs), body) ->
    Let (x, CallDir (Id.L ("min_caml_loop_start"), rargs, fargs), find_loop_end p body reg mem jargs)
  | Let (_, _, body) ->
    find_loop_start p body reg mem jargs
  | Ans (exp) ->
    begin match exp with
      | CallDir (id_l, argsr, _) when (contains (string_of_id_l id_l) "min_caml") ->
        Ans (exp)
      | CallDir (id_l, argsr, _) ->
        let fundef = find_fundef p id_l in
        let t = Inlining.inline_calldir_exp argsr fundef reg in
        find_loop_start p t reg mem jargs
      | IfEq _ | IfGE _ | IfLE _ ->
        find_loop_end_if p exp reg mem jargs
      | _ ->
        begin match Optimizer.optimize_exp p exp reg mem with
          | Specialized (v) ->
            let id = Id.gentmp Type.Int in
            Let ((id, Type.Int),
                 Set (value_of v),
                 Ans (Mov (id)))
          | Not_specialized (e, v) -> Ans (e)
        end
    end

and find_loop_end p t reg mem jargs =
  match t with
  | Ans (exp) ->
    begin match exp with
    | IfGE _ | IfLE _ | IfEq _ ->
      find_loop_end_if p exp reg mem jargs
    | CallDir (id_l, argsr, _) ->
      let fundef = find_fundef p id_l in
      let t = Inlining.inline_calldir_exp argsr fundef reg in
      find_loop_end p t reg mem jargs
    | _ ->
      begin match Optimizer.optimize_exp p exp reg mem with
      | Specialized (v) ->
        let id = Id.gentmp Type.Int in
        Let ((id, Type.Int),
             Set (value_of v),
             Ans (Mov (id)))
      |  Not_specialized (e, v) ->
        Ans (e)
      end
    end
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_end"), rargsr, fargs), body) ->
    Ans (CallDir (Id.L ("min_caml_loop_end"), rargsr, fargs))
  | Let ((dest, typ), exp, body) ->
    begin match Optimizer.optimize_exp p exp reg mem with
      | Specialized (v) ->
        reg.(int_of_id_t dest) <- v;
        find_loop_end p body reg mem jargs
      | Not_specialized (e, v) ->
        reg.(int_of_id_t dest) <- v;
        Let ((dest, typ), e, find_loop_end p body reg mem jargs)
    end
    
and find_loop_end_if p e reg mem jargs =
  match e with
  | IfLE (id_t, id_or_imm, t1, t2) when (is_opcode id_t) ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 <= r2
    then find_loop_end p t1 reg mem jargs
    else find_loop_end p t2 reg mem jargs
  | IfEq (id_t, id_or_imm, t1, t2) when (is_opcode id_t) ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 = r2
    then find_loop_end p t1 reg mem jargs
    else find_loop_end p t2 reg mem jargs
  | IfGE (id_t, id_or_imm, t1, t2) when (is_opcode id_t) ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 >= r2
    then find_loop_end p t1 reg mem jargs
    else find_loop_end p t2 reg mem jargs
  | IfEq (id_t, id_or_imm, t1, t2) ->
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1 = Array.copy reg in
    let regt2 = Array.copy reg in
    let memt1 = Array.copy mem in
    let memt2 = Array.copy mem in
    let t1' = find_loop_end p t1 regt1 memt1 jargs in
    let t2' = find_loop_end p t2 regt2 memt2 jargs in
    begin match r1, r2 with
      | Green (n1), Green (n2)
      | LightGreen (n1), LightGreen (n2)
      | Green (n1), LightGreen (n2)
      | LightGreen (n1), Green (n2) ->
        if n1 = n2 then t1' else t2'
      | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
        Ans (IfEq (id_t, C (n2), t1', t2'))
      | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
        let id_t2 = match id_or_imm with
            V (id) -> id
          | C (n) -> failwith "id_or_imm should be string"
        in
        Ans (IfEq (id_t2, C (n1), t1', t2'))
      | Red (n1), Red (n2) ->
        Ans (IfEq (id_t, id_or_imm, t1', t2'))
    end
  | IfLE (id_t, id_or_imm, t1, t2) ->
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1 = Array.copy reg in
    let regt2 = Array.copy reg in
    let memt1 = Array.copy mem in
    let memt2 = Array.copy mem in
    let t1' = find_loop_end p t1 regt1 memt1 jargs in
    let t2' = find_loop_end p t2 regt2 memt2 jargs in
    begin match r1, r2 with
      | Green (n1), Green (n2)
      | LightGreen (n1), LightGreen (n2)
      | Green (n1), LightGreen (n2)
      | LightGreen (n1), Green (n2) ->
        if n1 <= n2 then t1' else t2'
      | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
        Ans (IfLE (id_t, C (n2), t1', t2'))
      | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
        let id_t2 = match id_or_imm with
            V (id) -> id
          | C (n) -> failwith "id_or_imm should be string"
        in
        Ans (IfLE (id_t2, C (n1), t1', t2'))
      | Red (n1), Red (n2) ->
        Ans (IfLE (id_t, id_or_imm, t1', t2'))
    end
  | IfGE (id_t, id_or_imm, t1, t2) ->
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1 = Array.copy reg in
    let regt2 = Array.copy reg in
    let memt1 = Array.copy mem in
    let memt2 = Array.copy mem in
    let t1' = find_loop_end p t1 regt1 memt1 jargs in
    let t2' = find_loop_end p t2 regt2 memt2 jargs in
    begin match r1, r2 with
      | Green (n1), Green (n2)
      | LightGreen (n1), LightGreen (n2)
      | Green (n1), LightGreen (n2)
      | LightGreen (n1), Green (n2) ->
        if n1 >= n2 then t1' else t2'
      | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
        Ans (IfGE (id_t, C (n2), t1', t2'))
      | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
        let id_t2 = match id_or_imm with
            V (id) -> id
          | C (n) -> failwith "id_or_imm should be string"
        in
        Ans (IfGE (id_t2, C (n1), t1', t2'))
      | Red (n1), Red (n2) ->
        Ans (IfGE (id_t, id_or_imm, t1', t2'))
    end
  | _ -> failwith "method_jit_if should accept conditional branches."

    
