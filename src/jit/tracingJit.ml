open Asm
open Core
open JitConfig
open Renaming
open Inlining

exception Not_supported of string

module Guard = struct
  let select_branch e n1 n2 t1 t2 =
    match e with
    | IfEq _ ->
      if n1 = n2 then t1 else t2
    | IfLE _ ->
      if n1 <= n2 then t1 else t2
    | IfGE _ ->
      if n1 >= n2 then t1 else t2
    | _ ->
      failwith "Only IfEq, IfLE and IfGE should be come here."

  let rec get_free_vars = function
    | Ans (exp) -> get_free_vars' exp
    | Let ((dest, _), e, t) -> List.append (dest :: (get_free_vars' e)) (get_free_vars t)

  and get_free_vars' = function
    | Mov (id) -> [id]
    | Add (id_t, V (id)) | Sub (id_t, V (id)) ->  id_t :: id :: []
    | Add (id_t, C _) | Sub (id_t, C _) -> id_t :: []
    | Ld (dest, V (offset), _) -> dest :: offset :: []
    | Ld (dest, C (_), _) -> dest :: []
    | St (src, dest, V (offset), _) -> src :: dest :: offset :: []
    | St (src, dest, C (_), _) -> src :: dest :: []
    | IfEq (id_t1, V (id_t2), _, _) | IfLE (id_t1, V (id_t2), _, _) | IfGE (id_t1, V (id_t2), _, _) -> id_t1 :: id_t2 :: []
    | IfEq (id_t1, C (_), _, _) | IfLE (id_t1, C (_), _, _) | IfGE (id_t1, C (_), _, _) -> id_t1 :: []
    | CallDir (id_l, args, fargs) -> List.append args fargs
    | _ -> []

  let restore_green reg cont =
    let free_vars = get_free_vars cont in
    let rec restore cont = function
      | [] ->
        cont
      | hd :: tl ->
        match reg.(int_of_id_t hd) with
        | Green n ->
          Let ((hd, Type.Int), Set (n), restore cont tl)
        | Red _ ->
          restore cont tl
    in restore cont free_vars
end

open Guard

let rec tracing_jit (p : prog) (instr : t) (reg : value array) (mem : value array) (jit_args : jit_args) : t =
  match instr with
  | Ans (exp) ->
    tracing_jit_ans p exp reg mem jit_args
  | Let ((dest, typ), CallDir (id_l, argsr, argst), contbody) ->
    let fundef = rename_fundef (find_fundef p id_l) in
    inline_calldir argsr dest fundef (tracing_jit p contbody reg mem jit_args) reg
  | Let ((dest, typ), instr, body) ->
    (match tracing_jit_let p instr reg mem with
     | Specialized v ->
       reg.(int_of_id_t dest) <- v;
       tracing_jit p body reg mem jit_args
     | Not_specialized (e, v) ->
       reg.(int_of_id_t dest) <- v;
       Let ((dest, typ), e, tracing_jit p body reg mem jit_args))

and tracing_jit_ans (p : prog) (e : exp) (reg : value array) (mem : value array) (jit_args : jit_args) : t =
  match e with
  | CallDir (id_l, argsr, _) ->
    let fundef = find_fundef p id_l in
    let pc = value_of reg.(find_pc argsr jit_args) in
    (match (pc = (jit_args.loop_header)) with
     | true ->
       let reds = List.filter ~f:(fun a -> is_red reg.(int_of_id_t a)) argsr in
       Ans (CallDir (Id.L (jit_args.trace_name), reds, []))
     | false ->
       tracing_jit p (inline_calldir_exp argsr fundef reg) reg mem jit_args)
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
       failwith "if (green, red)"
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
  | _ -> Ans e

and tracing_jit_let (p : prog) (e : exp) (reg : value array) (mem : value array) : jit_result =
  match e with
  | Set n ->
    Specialized (Green n)
  | Mov id_t as exp ->
    let r = reg.(int_of_id_t id_t) in
    (match r with
     | Green (n) ->
       Specialized (Green (n))
     | Red (n) ->
       Not_specialized (exp, Red (n)))
  | Add (id_t1, id_or_imm) as exp ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 = match id_or_imm with
      | V (id_t) -> reg.(int_of_id_t id_t)
      | C (n) -> Green (n)
    in
    (match r1, r2 with
     | Green (n1), Green (n2) ->
       Specialized (Green (n1 + n2))
     | Red (n1), Green (n2) ->
       Not_specialized (Add (id_t1, C (n2)), Red (n1 + n2))
     | Green (n1), Red (n2) ->
       let id_t' = match id_or_imm with
           V (id) -> id
         | C (n) -> failwith "Add (green, red)"
       in
       Not_specialized (Add (id_t', C (n1)), Red (n1 + n2))
     | Red (n1), Red (n2) ->
       Not_specialized (exp, Red (n1 + n2)))
  | Sub (id_t1, id_or_imm) as exp ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 = match id_or_imm with
      | V (id_t) -> reg.(int_of_id_t id_t)
      | C (n) -> Green (n)
    in
    (match r1, r2 with
     | Green (n1), Green (n2) ->
       Specialized (Green (n1 - n2))
     | Red (n1), Green (n2) ->
       Not_specialized (Sub (id_t1, C (n2)), Red (n1 - n2))
     | Green (n1), Red (n2) ->
       let id_t' = match id_or_imm with
           V (id) -> id
         | C (n) -> failwith "Sub (green, red)"
       in
       Not_specialized (Add (id_t', C (n1)), Red (n1 - n2))
     | Red (n1), Red (n2) ->
       Not_specialized (exp, Red (n1 - n2)))
  | Ld (id_t, id_or_imm, x) as exp ->
    let destld = reg.(int_of_id_t id_t) in
    let offsetld =
      (match id_or_imm with
       | V (id_t) ->
         (match reg.(int_of_id_t id_t) with
          | Green (n1) -> Green (n1 * x)
          | Red (n1) -> Red (n1 * x))
       | C (n) -> Green (n * x))
    in
    (match destld, offsetld with
     | Green (n1), Green (n2) ->
       (match mem.(n1 + n2) with
        | Green n as value ->
          Specialized (value)
        | Red n ->
          let e = Ld (id_t, C (n2), x) in
          Not_specialized (e, Red n))
     | Green (n1), Red (n2) ->
       failwith "Ld (green, red)"
     | Red (n1), Green (n2) ->
       let n = mem.(n1 + n2) in
       Not_specialized (Ld (id_t, C (n2), x), n)
     | Red (n1), Red (n2) ->
       let n = mem.(n1 + n2) in
       Not_specialized (exp, n))
  | St (dest, src, offset, x) as exp ->
    let dest', src' = reg.(int_of_id_t dest), reg.(int_of_id_t src) in
    let offset' = match offset with
      | V (id_t) ->
        (match reg.(int_of_id_t id_t) with
         | Green (n) -> Green (n * x)
         | Red (n) -> Red (n * x))
      | C (n) -> Green (n * x)
    in
    (match src', offset' with
     | Green (n1), Green (n2) ->
       mem.(n1 + n2) <- dest';
       Specialized (Green (0))
     | Green (n1), Red (n2) ->
       failwith "St (green, red)"
     | Red (n1), Green (n2) ->
       mem.(n1 + n2) <- dest';
       Not_specialized (St (dest, src, C (n2), x), Red (0))
     | Red (n1), Red (n2) ->
       mem.(n1 + n2) <- dest';
       Not_specialized (exp, Red (0)))
  | _ ->
    failwith "Not supported."

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
