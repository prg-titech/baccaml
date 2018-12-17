open Core
open MinCaml
open Asm
open Inlining
open Renaming
open Operands
open Jit_config
open Jit_util

type tj_env =
  { index_pc: int; merge_pc: int; trace_name: string }
[@@deriving fields]

let rec unique list =
  let rec go l s =  match l with
      [] -> s
    | first :: rest ->
      if List.exists ~f:(fun e -> e = first) s
      then go rest s
      else go rest (s @ [first])
  in go list []

let find_pc { index_pc; } args =
  match List.nth args index_pc with
  | Some (s) -> int_of_id_t s
  | None -> failwith "find_pc is failed"

let find_fundef name prog =
  let Prog (_, fundefs, _) = prog in
  match List.find ~f:(fun fundef -> fundef.name = name) fundefs with
  | Some (body) -> body
  | None ->
    let Id.L (x) = name in
    failwith @@ Printf.sprintf "find_fundef is failed: %s" x

let rec connect (id_t, typ) instr body =
  let rec go id_t  body = function
    | Let (a, e, t) -> Let (a, e, go id_t t body)
    | Ans e -> Let ((id_t, typ), e, body)
  in go id_t instr body

module Guard = struct
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

  let rec add_guard_label reg cont tj_env = function
    | Ans (CallDir (Id.L (x), args, fargs)) ->
      let { index_pc; merge_pc; trace_name } = tj_env in
      let pc  = match List.nth args index_pc with
        | Some (id) -> reg.(int_of_id_t id) |> value_of
        | None -> failwith (Printf.sprintf "specified index_pc is invalid: %d" index_pc)
      in
      if pc = merge_pc then
        Ans ((CallDir (Id.L (trace_name), args, fargs)))
      else
        Ans ((CallDir (Id.L ("guard_failure." ^ x), args, fargs)))
    | Ans (exp) -> Ans (exp)
    | Let ((id, typ), exp, body) ->
      Let ((id, typ), exp, add_guard_label reg cont tj_env body)

  let restore_green reg cont tj_env =
    let free_vars = unique (get_free_vars cont) in
    let rec restore cont = function
      | [] -> cont
      | hd :: tl ->
        match reg.(int_of_id_t hd) with
        | Green n ->
          Let ((hd, Type.Int), Set (n), restore cont tl)
        | _ ->
          restore cont tl
    in restore cont free_vars |> add_guard_label reg cont tj_env
end

let rec tj (p : prog) (reg : value array) (mem : value array) (tj_env : tj_env) = function
  | Ans (exp) -> tj_exp p reg mem tj_env exp
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) ->
    let fundef = find_fundef id_l p in
    let t = Inlining.inline_calldir_exp args fundef reg in
    let t' = tj p reg mem tj_env t  in
    connect (dest, typ) t' (tj p reg mem tj_env body)
  | Let ((dest, typ), exp, body) ->
    match exp with
    | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) | IfGE (id_t, id_or_imm, t1, t2) ->
      let r1 = reg.(int_of_id_t id_t) in
      let r2 = match id_or_imm with
        | V (id) -> reg.(int_of_id_t id)
        | C (n) -> Green (n)
      in
      let id = match id_or_imm with
        | V (id) -> id
        | C (n) -> string_of_int n
      in
      let n1, n2 = value_of r1, value_of r2 in
      reg.(int_of_id_t id_t) <- Red (0);
      begin match r1, r2 with
        | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
          connect (dest, typ)
            (if exp |*| (n1, n2)
             then (Ans (exp |%| (id_t, C (n2), tj p reg mem tj_env t1, Guard.restore_green reg t2 tj_env)))
             else (Ans (exp |%| (id_t, C (n2), Guard.restore_green reg t1 tj_env, tj p reg mem tj_env t2)))
            ) (tj p reg mem tj_env body)
        | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
          connect (dest, typ)
            (if exp |*| (n1, n2)
             then (Ans (exp |%| (id, C (n1), tj p reg mem tj_env t2, Guard.restore_green reg t1 tj_env)))
             else (Ans (exp |%| (id, C (n1), Guard.restore_green reg t2 tj_env, tj p reg mem tj_env t1)))
            ) (tj p reg mem tj_env body)
        | Red (n1), Red (n2) ->
          connect (dest, typ)
            (if exp |*| (n1, n2)
             then (Ans (exp |%| (id_t, id_or_imm, tj p reg mem tj_env t1, Guard.restore_green reg t2 tj_env)))
             else (Ans (exp |%| (id_t, id_or_imm, Guard.restore_green reg t1 tj_env, tj p reg mem tj_env t2)))
            ) (tj p reg mem tj_env body)
        | _ -> tj p reg mem tj_env (if exp |*| (n1, n2) then t1 else t2)
      end
    | Ld (id_t, id_or_imm, x) ->
      let r1 = int_of_id_t id_t |> Array.get reg in
      let r2 = match id_or_imm with V (id) -> int_of_id_t id_t |> Array.get reg | C (n) -> Green (n) in
      begin match r1, r2 with
        | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
          let n = mem.(n1 + n2 * x) in
          let id_t2 = match id_or_imm with V (id) -> id | C (n) -> failwith "id_or_imm should be string" in
          reg.(int_of_id_t id_t) <- n;
          Let ((dest, typ), Ld (id_t, id_or_imm, x), tj p reg mem tj_env body)
        | _ ->
          optimize_exp p reg mem tj_env (dest, typ) body exp
      end
    | St (id_t1, id_t2, id_or_imm, x) ->
      let srcv = reg.(int_of_id_t id_t1) in
      let destv = reg.(int_of_id_t id_t2) in
      let offsetv = match id_or_imm with
        | V (id) -> reg.(int_of_id_t id)
        | C (n) -> Green (n) in
      let body' = tj p reg mem tj_env body in
      begin match srcv, destv with
        | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
          reg.(int_of_id_t dest) <- Green (0);
          mem.(n1 + n2 * x) <- Green (int_of_id_t id_t1 |> Array.get reg |> value_of);
          begin match offsetv with
            | Green (n) | LightGreen (n) ->
              let id' = Id.gentmp Type.Int in
              Let ((id_t1, Type.Int), Set (n1),
                   Let ((id', Type.Int), Set (n),
                        Let ((dest, typ), St (id_t1, id_t2, C (n), x), body')))
            | Red (n) ->
              Let ((id_t1, Type.Int), Set (n1),
                   Let ((dest, typ), St (id_t1, id_t2, id_or_imm, x), body'))
          end
        | _ -> optimize_exp p reg mem tj_env (dest, typ) body exp
      end
    | _ -> optimize_exp p reg mem tj_env (dest, typ) body exp

and optimize_exp p reg mem tj_env (dest, typ) body exp =
  match Jit_optimizer.run p exp reg mem with
  | Specialized v ->
    reg.(int_of_id_t dest) <- v;
    tj p reg mem tj_env body
  | Not_specialized (e, v) ->
    reg.(int_of_id_t dest) <- v;
    Let ((dest, typ), e, tj p reg mem tj_env body)

and tj_exp (p : prog) (reg : value array) (mem : value array) (tj_env : tj_env) = function
  | CallDir (id_l, args, _) ->
    Logs.debug (fun m -> m "CallDir (%s)" (string_of_id_l id_l));
    let fundef =  find_fundef id_l p in
    let pc = args |> find_pc tj_env |> Array.get reg |> value_of in
    Logs.debug (fun m -> m "pc : %d" pc);
    let { merge_pc; trace_name } = tj_env in
    if pc = merge_pc then
      let reds = args |> List.filter ~f:(fun a ->
          is_red reg.(int_of_id_t a))
      in
      Ans (CallDir (Id.L (trace_name), reds, []))
    else
      Inlining.inline_calldir_exp args fundef reg |> tj p reg mem tj_env
  | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) | IfGE (id_t, id_or_imm, t1, t2) as exp ->
    tj_if p reg mem tj_env exp
  | exp ->
    begin match Jit_optimizer.run p exp reg mem with
      | Specialized (v) -> Ans (Set (value_of v))
      | Not_specialized (e, v) -> Ans (e)
    end

and tj_if (p : prog) (reg : value array) (mem : value array) (tj_env : tj_env) = function
  | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) | IfGE (id_t, id_or_imm, t1, t2) as exp ->
    Logs.debug begin fun m ->
      let if_rep = match exp with
        | IfEq _ -> "IfEq"
        | IfLE _ -> "IfLE"
        | IfGE _ -> "IfGE"
        | _ -> failwith "If expression should be come here."
      in
      m "%s (%s, %s)" if_rep id_t (string_of_id_or_imm id_or_imm)
    end;
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> reg.(int_of_id_t id)
      | C (n) -> Green (n)
    in
    begin match r1, r2 with
      | Green (n1), Green (n2) | LightGreen (n1), Green (n2) | Green (n1), LightGreen (n2) | LightGreen (n1), LightGreen (n2) ->
        if exp |*| (n1, n2)
        then tj p reg mem tj_env t1
        else tj p reg mem tj_env t2
      | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
        if exp |*| (n1, n2) then
          Ans (exp |%| (id_t, C (n2), tj p reg mem tj_env t1, Guard.restore_green reg t2 tj_env))
        else
          Ans (exp |%| (id_t, C (n2), Guard.restore_green reg t1 tj_env, tj p reg mem tj_env t2))
      | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
        let id_r2 = match id_or_imm with V (id) -> id | C (n) -> failwith "id_or_imm should be string" in
        if exp |*| (n1, n2) then
          Ans (exp |%| (id_r2, C (n1), tj p reg mem tj_env t1, Guard.restore_green reg t2 tj_env))
        else
          Ans (exp |%| (id_r2, C (n1), Guard.restore_green reg t1 tj_env, tj p reg mem tj_env t2))
      | Red (n1), Red (n2) ->
        if exp |*| (n1, n2) then
          Ans (exp |%| (id_t, id_or_imm, tj p reg mem tj_env t1, Guard.restore_green reg t2 tj_env))
        else
          Ans (exp |%| (id_t, id_or_imm, Guard.restore_green reg t1 tj_env, tj p reg mem tj_env t2))
    end
  | e ->
    begin match Jit_optimizer.run p e reg mem with
      | Specialized (v) -> Ans (Set (value_of v))
      | Not_specialized (e, v) -> Ans (e)
    end

let run_while p reg mem name reds index_pc merge_pc =
  let Prog (tbl, _, m) = p in
  let Jit_prep.Env (fdfs, ibody, reds) =
    Jit_prep.prep ~prog:p ~name:name ~red_args:reds ~jit_type:`Meta_tracing in
  let p' = Prog (tbl, fdfs, m) in
  let trace = tj p' reg mem { index_pc = index_pc; merge_pc = merge_pc; trace_name = name } ibody in
  { name = Id.L (name); args = reds; fargs = []; body = trace; ret = Type.Int }
