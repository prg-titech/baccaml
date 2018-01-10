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
end

type method_jit_result =
  | MSpecialized of value
  | MNot_specialized of exp * value

let is_first_calldir = ref true

let rec method_jit p instr reg mem jit_args = match instr with
  | Ans (exp) ->
    method_jit_ans p exp reg mem jit_args
  | Let ((dest, typ), exp, body) ->
    begin
      match TracingJit.tracing_jit_let p exp reg mem with
      | Specialized (v) ->
        method_jit p body reg mem jit_args
      | Not_specialised (e, v) ->
        reg.(int_of_id_t dest) <- v;
        Let ((dest, typ), exp, method_jit p body reg mem jit_args)
    end

and method_jit_let p e reg mem = match e with
  | Set n ->
    MSpecialized (Green n)
  | Mov id ->
    begin
      match reg.(int_of_id_t id) with
      | Green n as r -> MSpecialized (r)
      | Red n as r -> MNot_specialized (e, r)
    end
  | Add (id_t1, id_or_imm) ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 = match id_or_imm with
      | V (id_t) -> reg.(int_of_id_t id_t)
      | C (n) -> Green (n)
    in
    begin
      match r1, r2 with
      | Green (n1), Green (n2) ->
        MSpecialized (Green (n1 + n2))
      | Red (n1), Green (n2) ->
        MNot_specialized (Add (id_t1, C (n2)), Red (n1 + n2))
      | Green (n1), Red (n2) ->
        failwith "Add (green, red)"
      | Red (n1), Red (n2) ->
        MNot_specialized (e, Red (n1 + n2))
    end
  | Ld (id_t, id_or_imm, x) ->
    let destld = reg.(int_of_id_t id_t) in
    let offsetld =
      (match id_or_imm with
       | V (id_t) ->
         (match reg.(int_of_id_t id_t) with
          | Green (n1) -> Green (n1 * x)
          | Red (n1) -> Red (n1 * x))
       | C (n) -> Green (n * x))
    in
    begin
      match destld, offsetld with
      | Green (n1), Green (n2) ->
        (match mem.(n1 + n2) with
         | Green n as value ->
           MSpecialized (value)
         | Red n ->
           MNot_specialized (Ld (id_t, C (n2), x), Red n))
      | Green (n1), Red (n2) ->
        failwith "Ld (green, red)"
      | Red (n1), Green (n2) ->
        let n = mem.(n1 + n2) in
        MNot_specialized (Ld (id_t, C (n2), x), n)
      | Red (n1), Red (n2) ->
        let n = mem.(n1 + n2) in
        MNot_specialized (e, n)
    end
  | St (dest, src, offset, x) ->
    let dest', src' = reg.(int_of_id_t dest), reg.(int_of_id_t src) in
    let offset' = match offset with
      | V (id_t) ->
        (match reg.(int_of_id_t id_t) with
         | Green (n) -> Green (n * x)
         | Red (n) -> Red (n * x))
      | C (n) -> Green (n * x)
    in
    begin match src', offset' with
      | Green (n1), Green (n2) ->
        mem.(n1 + n2) <- dest';
        MSpecialized (Green (0))
      | Green (n1), Red (n2) ->
        failwith "St (green, red)"
      | Red (n1), Green (n2) ->
        mem.(n1 + n2) <- dest';
        MNot_specialized (St (dest, src, C (n2), x), Red (0))
      | Red (n1), Red (n2) ->
        mem.(n1 + n2) <- dest';
        MNot_specialized (e, Red (0))
    end
  | _ -> failwith "Not supported in method jit"

and method_jit_ans p e reg mem jit_args = match e with
  | CallDir (id_l, argsr, _) ->
    let fundef = find_fundef p id_l in
    if !is_first_calldir then
      (is_first_calldir := false; method_jit p (inline_calldir_exp argsr fundef reg) reg mem jit_args)
    else Ans e
  | IfEq (id_t, id_or_imm, t1, t2) ->
    let r2 = Util.value_of_id_or_imm reg id_or_imm in
    Ans (
      match r2 with
      | Green (n2) ->
        let reg', mem' = reg, mem in
        let t1' = method_jit p t1 reg' mem' jit_args in
        let t2' = method_jit p t2 reg' mem' jit_args in
        IfEq (id_t, C (n2), t1', t2')
      | Red (n2) ->
        let reg', mem' = reg, mem in
        let t1' = method_jit p t1 reg' mem' jit_args in
        let t2' = method_jit p t2 reg' mem' jit_args in
        IfEq (id_t, id_or_imm, t1', t2')
    )
  | IfLE (id_t, id_or_imm, t1, t2) ->
    let r2 = Util.value_of_id_or_imm reg id_or_imm in
    Ans (
      match r2 with
      | Green (n2) ->
        let reg', mem' = reg, mem in
        let t1' = method_jit p t1 reg' mem' jit_args in
        let t2' = method_jit p t2 reg' mem' jit_args in
        IfLE (id_t, C (n2), t1', t2')
      | Red (n2) ->
        let reg', mem' = reg, mem in
        let t1' = method_jit p t1 reg' mem' jit_args in
        let t2' = method_jit p t2 reg' mem' jit_args in
        IfLE (id_t, id_or_imm, t1', t2')
    )
  | IfGE (id_t, id_or_imm, t1, t2) ->
    let r2 = Util.value_of_id_or_imm reg id_or_imm in
    Ans (
      match r2 with
      | Green (n2) ->
        let reg', mem' = reg, mem in
        let t1' = method_jit p t1 reg' mem' jit_args in
        let t2' = method_jit p t2 reg' mem' jit_args in
        IfGE (id_t, C (n2), t1', t2')
      | Red (n2) ->
        let reg', mem' = reg, mem in
        let t1' = method_jit p t1 reg' mem' jit_args in
        let t2' = method_jit p t2 reg' mem' jit_args in
        IfGE (id_t, id_or_imm, t1', t2')
    )
  | _ -> Ans (e)
