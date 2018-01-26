open Asm
open Core
open Inlining
open Jit_config
open Renaming

let find_pc argsr n = int_of_id_t (List.nth_exn argsr n)

let value_of_id_t reg id_t = reg.(int_of_id_t id_t)

let value_of_id_or_imm reg = function
  | V (id) -> reg.(int_of_id_t id)
  | C (n) -> Green (n)

let name_of id = List.hd_exn (String.split id ~on:'.')

let print_value = function
  | Green (n) -> Format.eprintf "Green (%d)" n
  | Red (n) -> Format.eprintf "Red (%d)" n

let rec method_jit : prog -> t -> reg -> mem -> method_jit_args -> t =
  fun p instr reg mem method_jit_args -> match instr with
  | Ans (exp) ->
    method_jit_ans p exp reg mem method_jit_args
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) ->
    begin
      let rec go cont = function
          [] -> cont
        | hd :: tl ->
         if is_green reg.(int_of_id_t hd) then
            Let ((hd, Type.Int),
                 Set (value_of reg.(int_of_id_t hd)),
                 go cont tl)
          else go cont tl
      in
      let t =
        Let ((dest, typ),
             CallDir (id_l, args, fargs),
             (method_jit p body reg mem method_jit_args))
      in go t args
    end
  | Let ((dest, typ), exp, body) ->
    begin
      match Tracing_jit.optimize_exp p exp reg mem with
      | Specialized (v) ->
        reg.(int_of_id_t dest) <- v;
        method_jit p body reg mem method_jit_args
      | Not_specialized (e, v) ->
        reg.(int_of_id_t dest) <- v;
        Let ((dest, typ),
             e,
             method_jit p body reg mem method_jit_args)
    end

and method_jit_ans p e reg mem method_jit_args = match e with
  | CallDir (Id.L ("min_caml_print_int"),  _, _)
  | CallDir (Id.L ("min_caml_print_string"), _, _)
  | CallDir (Id.L ("min_caml_print_newline"), _, _) -> Ans (e)
  | CallDir (id_l, argsr, _) ->
    let fundef = find_fundef p id_l in
    let t' = Inlining.inline_calldir_exp argsr fundef reg in
    method_jit p t' reg mem method_jit_args
  | IfLE (id_t, id_or_imm, t1, t2) when ((name_of id_t) = "instr") ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 <= r2 then method_jit p t1 reg mem method_jit_args
    else method_jit p t2 reg mem method_jit_args
  | IfEq (id_t, id_or_imm, t1, t2) when ((name_of id_t) = "instr") ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 = r2 then method_jit p t1 reg mem method_jit_args
    else method_jit p t2 reg mem method_jit_args
  | IfGE (id_t, id_or_imm, t1, t2) when ((name_of id_t) = "instr") ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 >= r2 then method_jit p t1 reg mem method_jit_args
    else method_jit p t2 reg mem method_jit_args
  | IfEq (id_t, id_or_imm, t1, t2) ->
    let r2 = value_of_id_or_imm reg id_or_imm in
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
    let r2 = value_of_id_or_imm reg id_or_imm in
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
    let r2 = value_of_id_or_imm reg id_or_imm in
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
  | _ ->
    begin
      match Tracing_jit.optimize_exp p e reg mem with
      | Specialized (v) ->
        Ans (e)
      | Not_specialized (e, v) ->
        Ans (e)
    end

let exec_method_jit p instr reg mem method_jit_args =
  let { method_name; reds } = method_jit_args in
  let res = method_jit p instr reg mem method_jit_args in
  { name = Id.L (method_name)
  ; args = reds
  ; fargs = []
  ; body = res
  ; ret = Type.Int
  }
