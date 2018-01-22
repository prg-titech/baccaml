open Asm
open Core
open Inlining
open JitConfig
open Renaming

module Util = struct
  let find_pc argsr n = int_of_id_t (List.nth_exn argsr n)

  let value_of_id_t reg id_t = reg.(int_of_id_t id_t)

  let value_of_id_or_imm reg = function
    | V (id) -> reg.(int_of_id_t id)
    | C (n) -> Green (n)

  let name_of id = List.hd_exn (String.split id ~on:'.')
end

let print_value = function
  | Green (n) -> Format.eprintf "Green (%d)" n
  | Red (n) -> Format.eprintf "Red (%d)" n

type method_jit_result =
  | MSpecialized of value
  | MNot_specialized of exp * value

type method_jit_args =
  { method_name : string
  ; reds : string list
  ; method_start : int
  ; method_end : int
  ; pc_place : int
  }

let can_enter_else = ref true

let inline_calldir argsr fundef reg =
  let { args; body } = fundef in
  Inlining.inline_args argsr args body reg

let rec method_jit p instr reg mem method_jit_args =
  match instr with
  | Ans (exp) ->
    method_jit_ans p exp reg mem method_jit_args
  | Let ((dest, typ), exp, body) ->
    begin
      match TracingJit.tracing_jit_let p exp reg mem with
      | Specialized (v) ->
        reg.(int_of_id_t dest) <- v;
        method_jit p body reg mem method_jit_args
      | Not_specialized (e, v) ->
        reg.(int_of_id_t dest) <- v;
        Let ((dest, typ), exp, method_jit p body reg mem method_jit_args)
    end

and method_jit_ans p e reg mem method_jit_args = match e with
  | CallDir (id_l, argsr, _) ->
    let { method_name; reds; method_end; pc_place } = method_jit_args in
    let fundef = find_fundef p id_l in
    let pc = value_of reg.(Util.find_pc argsr pc_place) in
    let t' = inline_calldir argsr fundef reg in
    begin
      match pc <> (method_end) with
        true ->
        method_jit p t' reg mem method_jit_args
      | false ->
        Ans (CallDir (Id.L (method_name), reds, []))
    end
  | IfLE (id_t, id_or_imm, t1, t2) when ((Util.name_of id_t) = "instr") ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 <= r2 then method_jit p t1 reg mem method_jit_args
    else method_jit p t2 reg mem method_jit_args
  | IfEq (id_t, id_or_imm, t1, t2) when ((Util.name_of id_t) = "instr") ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 = r2 then method_jit p t1 reg mem method_jit_args
    else method_jit p t2 reg mem method_jit_args

  | IfEq (id_t, id_or_imm, t1, t2) ->
    let r2 = Util.value_of_id_or_imm reg id_or_imm in
    Ans (
      match r2 with
      | Green (n2) ->
        let regt1 = Array.copy reg in
        let regt2 = Array.copy reg in
        let memt1 = Array.copy mem in
        let memt2 = Array.copy mem in
        let t1' = method_jit p t1 regt1 memt1 method_jit_args in
        let t2' = method_jit p t2 regt2 memt2 method_jit_args in

        IfEq (id_t, C (n2), t1', t2')
      | Red (n2) ->
        let regt1 = Array.copy reg in
        let regt2 = Array.copy reg in
        let memt1 = Array.copy mem in
        let memt2 = Array.copy mem in
        let t1' = method_jit p t1 regt1 memt1 method_jit_args in
        let t2' = method_jit p t2 regt2 memt2 method_jit_args in

        IfEq (id_t, id_or_imm, t1', t2')
    )
  | IfLE (id_t, id_or_imm, t1, t2) ->
    let r2 = Util.value_of_id_or_imm reg id_or_imm in
    Ans (
      match r2 with
      | Green (n2) ->
        let regt1 = Array.copy reg in
        let regt2 = Array.copy reg in
        let memt1 = Array.copy mem in
        let memt2 = Array.copy mem in
        let t1' = method_jit p t1 regt1 memt1 method_jit_args in
        let t2' = method_jit p t2 regt2 memt2 method_jit_args in
        IfLE (id_t, C (n2), t1', t2')
      | Red (n2) ->
        let regt1 = Array.copy reg in
        let regt2 = Array.copy reg in
        let memt1 = Array.copy mem in
        let memt2 = Array.copy mem in
        let t1' = method_jit p t1 regt1 memt1 method_jit_args in
        let t2' = method_jit p t2 regt2 memt2 method_jit_args in
        IfLE (id_t, id_or_imm, t1', t2')
    )
  | IfGE (id_t, id_or_imm, t1, t2) ->
    let r2 = Util.value_of_id_or_imm reg id_or_imm in
    Ans (
      match r2 with
      | Green (n2) ->
        let regt1 = Array.copy reg in
        let regt2 = Array.copy reg in
        let memt1 = Array.copy mem in
        let memt2 = Array.copy mem in
        let t1' = method_jit p t1 regt1 memt1 method_jit_args in
        let t2' = method_jit p t2 regt2 memt2 method_jit_args in
        IfGE (id_t, C (n2), t1', t2')
      | Red (n2) ->
        let regt1 = Array.copy reg in
        let regt2 = Array.copy reg in
        let memt1 = Array.copy mem in
        let memt2 = Array.copy mem in
        let t1' = method_jit p t1 regt1 memt1 method_jit_args in
        let t2' = method_jit p t2 regt2 memt2 method_jit_args in
        IfGE (id_t, id_or_imm, t1', t2')
    )
  | _ -> Ans (e)
