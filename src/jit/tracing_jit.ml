open Asm
open Core
open Guard
open Jit_config
open Renaming
open Inlining

exception Not_supported of string

let bac_caml_nop_id = "bac_caml_nop_id.9999"

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
    (match optimize_exp p instr reg mem with
     | Specialized v ->
       reg.(int_of_id_t dest) <- v;
       tracing_jit p body reg mem jit_args
     | Not_specialized (e, v) ->
       reg.(int_of_id_t dest) <- v;
       Let ((dest, typ), e, tracing_jit p body reg mem jit_args))

and tracing_jit_ans p e reg mem jit_args = match e with
  | CallDir (id_l, args, _) ->
    let fundef = find_fundef p id_l in
    let pc = value_of reg.(find_pc args jit_args) in
    begin match (pc = (jit_args.loop_header)) with
      | true ->
        let reds = List.filter ~f:(fun a -> is_red reg.(int_of_id_t a)) args in
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
      match optimize_exp p e reg mem with
      | Specialized (v) ->
        Ans (Nop)
      | Not_specialized (e, v) ->
        Ans (e)
    end

and optimize_exp : prog -> exp -> reg -> mem-> jit_result =
  fun p e reg mem -> match e with
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
    let id_t2 = match id_or_imm with V (id) -> id | C (n) -> string_of_int n in
    (match r1, r2 with
     | Green (n1), Green (n2) ->
       Format.printf "Add (%s, %s), %d %d: Green, Green\n"
         id_t1 id_t2 (value_of r1) (value_of r2);
       Specialized (Green (n1 + n2))
     | Red (n1), Green (n2) ->
       Format.printf "Add (%s, %s), %d %d; Red, Green\n"
         id_t1 id_t2 (value_of r1) (value_of r2);
       Not_specialized (Add (id_t1, C (n2)), Red (n1 + n2))
     | Green (n1), Red (n2) ->
       Format.printf "Add (%s, %s), %d %d; Green, Red\n"
         id_t1 id_t2 (value_of r1) (value_of r2);
       let id_t' = match id_or_imm with
           V (id) -> id
         | C (n) -> failwith "Add (green, red)"
       in
       Not_specialized (Add (id_t', C (n1)), Red (n1 + n2))
     | Red (n1), Red (n2) ->
       Format.printf "Add (%s, %s), %d %d; Red, Red\n"
         id_t1 id_t2 (value_of r1) (value_of r2);
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
    let id_t2 = match id_or_imm with V (id) -> id | C (n) -> string_of_int n in
    (match destld, offsetld with
     | Green (n1), Green (n2) ->
       begin match mem.(n1 + n2) with
        | Green n as value ->
          Format.printf "Ld (%s, %s), %d %d => %d (Green): Green, Green\n"
            id_t id_t2 (value_of destld) (value_of offsetld) n;
          Specialized (value)
        | Red n ->
          Format.printf "Ld (%s, %s), %d %d => %d (Red): Green, Green\n"
            id_t id_t2 (value_of destld) (value_of offsetld) n;
          let e = Ld (bac_caml_nop_id, C (n1 + n2), x) in
          Not_specialized (e, Red n)
       end
     | Green (n1), Red (n2) -> failwith "Ld (green, red)"
     | Red (n1), Green (n2) ->
       let n = mem.(n1 + n2) in
       Format.printf "Ld (%s, %s), %d %d => %d: Red, Green\n"
         id_t id_t2 (value_of destld) (value_of offsetld) (value_of n);
       begin match mem.(n1 + n2) with
         | Green (n) ->
           Not_specialized (Ld (id_t, C (n2), x), Red (n))
         | Red (n) ->
           Not_specialized (Ld (id_t, C (n2), x), Red (n))
       end
     | Red (n1), Red (n2) ->
       let n = mem.(n1 + n2) in
       Format.printf "Ld (%s, %s), %d %d => %d: Red, Red\n"
         id_t id_t2 (value_of destld) (value_of offsetld) (value_of n);
       Not_specialized (exp, Red (value_of n)))
  | St (src, dest, offset, x) ->
    let src' =reg.(int_of_id_t src) in
    let dest' = reg.(int_of_id_t dest) in
    let offset' = match offset with
      | V (id_t) ->
        (match reg.(int_of_id_t id_t) with
         | Green (n) -> Green (n * x)
         | Red (n) -> Red (n * x))
      | C (n) -> Green (n * x)
    in
    (* dest が green か red で命令を残すか残さないか決める *)
    begin
      match dest', offset' with
      | Green (n1), Green (n2) ->
        begin
          match src' with
            Green (n) ->
            mem.(n1 + n2) <- src';
            Specialized (Green (0))
          | Red (n) ->
            Not_specialized (St (src, bac_caml_nop_id, C (n1 + n2), x), Red (n))
        end
      | Green (n1), Red (n2) ->
        failwith "St (green, red)"
      | Red (n1), Green (n2) ->
        begin
          match src' with
          | Green (n) ->
            mem.(n1 + n2) <- src';
            Not_specialized (St (src, dest, C (n2), x), Red (0))
          | Red (n) ->
            mem.(n1 + n2) <- src';
            Not_specialized (St (src, bac_caml_nop_id, C (n1 + n2), x), Red (0))
        end
      | Red (n1), Red (n2) ->
        begin
          match src' with
          | Green (n) ->
            mem.(n1 + n2) <- Red (value_of src');
            Not_specialized (St (src, dest, C (n2), x), Red (0))
          | Red (n) ->
            mem.(n1 + n2) <- src';
            Not_specialized (St (src, bac_caml_nop_id, C (n1 + n2), x), Red (0))
        end
    end
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
