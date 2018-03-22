open Asm
open Core
open Inlining
open Jit_config
open Renaming

let find_pc argsr n = int_of_id_t (List.nth_exn argsr n)

let jit_value_of_id_t reg id_t = reg.(int_of_id_t id_t)

let jit_value_of_id_or_imm reg = function
  | V (id) -> reg.(int_of_id_t id)
  | C (n) -> Green (n)

let name_of id = List.hd_exn (String.split id ~on:'.')

let print_value = function
  | Green (n) -> Format.printf "Green (%d)" n
  | LightGreen (n) -> Format.printf "LightGreen (%d)" n
  | Red (n) -> Format.printf "Red (%d)" n

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
        match Optimizer.optimize_exp p exp reg mem with
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
    if r1 <= r2
    then method_jit p t1 reg mem method_jit_args
    else method_jit p t2 reg mem method_jit_args
  | IfEq (id_t, id_or_imm, t1, t2) when ((name_of id_t) = "instr") ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 = r2
    then method_jit p t1 reg mem method_jit_args
    else method_jit p t2 reg mem method_jit_args
  | IfGE (id_t, id_or_imm, t1, t2) when ((name_of id_t) = "instr") ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 >= r2
    then method_jit p t1 reg mem method_jit_args
    else method_jit p t2 reg mem method_jit_args
  | IfEq (id_t, id_or_imm, t1, t2) ->
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1 = Array.copy reg in
    let regt2 = Array.copy reg in
    let memt1 = Array.copy mem in
    let memt2 = Array.copy mem in
    let t1' = method_jit p t1 regt1 memt1 method_jit_args in
    let t2' = method_jit p t2 regt2 memt2 method_jit_args in
    begin match r1, r2 with
      | Green (n1), Green (n2) | LightGreen (n1), LightGreen (n2) | Green (n1), LightGreen (n2) | LightGreen (n1), Green (n2) ->
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
    let t1' = method_jit p t1 regt1 memt1 method_jit_args in
    let t2' = method_jit p t2 regt2 memt2 method_jit_args in
    begin match r1, r2 with
      | Green (n1), Green (n2) | LightGreen (n1), LightGreen (n2) | Green (n1), LightGreen (n2) | LightGreen (n1), Green (n2) ->
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
    let t1' = method_jit p t1 regt1 memt1 method_jit_args in
    let t2' = method_jit p t2 regt2 memt2 method_jit_args in
    begin match r1, r2 with
      | Green (n1), Green (n2) | LightGreen (n1), LightGreen (n2) | Green (n1), LightGreen (n2) | LightGreen (n1), Green (n2) ->
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
  | _ ->
    begin
      match Optimizer.optimize_exp p e reg mem with
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
