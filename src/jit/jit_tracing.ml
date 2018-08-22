open MinCaml
open Asm
open Inlining
open Renaming
open Operands
open Jit_config
open Jit_util

type tj_env = { index_pc: int; merge_pc: int; trace_name: string }

let (<=>) exp (n1, n2) = match exp with
  | IfEq _ -> n1 = n2
  | IfLE _ -> n1 <= n2
  | IfGE _ -> n1 >= n2
  | _ -> assert false

let find_pc { index_pc; } args =
  match List.nth_opt args index_pc with
  | Some (s) -> int_of_id_t s
  | None -> failwith "find_pc is failed"

let find_fundef name prog =
  let Prog (_, fundefs, _) = prog in
  match List.find_opt (fun fundef -> fundef.name = name) fundefs with
  | Some (body) -> body
  | None ->
    let Id.L (x) = name in
    failwith @@ Printf.sprintf "find_fundef is failed: %s" x

let rec connect (id_t, typ) instr body =
  let rec go id_t  body = function
    | Let (a, e, t) -> Let (a, e, go id_t t body)
    | Ans e -> Let ((id_t, typ), e, body)
  in go id_t instr body

let rec tj (p : prog) (reg : value array) (mem : value array) (tj_env : tj_env) = function
  | Ans (exp) -> tj_exp p reg mem tj_env exp
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) ->
    let fundef = find_fundef id_l p in
    Inlining.inline_calldir_exp args fundef reg
    |> tj p reg mem tj_env
    |> (tj p reg mem tj_env body |> connect (dest, typ))
  | Let ((dest, typ), instr, body) ->
    begin match Jit_optimizer.run p instr reg mem with
      | Specialized v ->
        reg.(int_of_id_t dest) <- v;
        tj p reg mem tj_env body
      | Not_specialized (e, v) ->
        reg.(int_of_id_t dest) <- v;
        Let ((dest, typ), e, tj p reg mem tj_env body)
    end

and tj_exp (p : prog) (reg : value array) (mem : value array) (tj_env : tj_env) = function
  | CallDir (id_l, args, _) ->
    let fundef =  find_fundef id_l p in
    let pc = args |> find_pc tj_env in
    let { merge_pc; trace_name } = tj_env in
    if pc = merge_pc then
      let reds = args |> List.filter (fun a -> is_red reg.(int_of_id_t a)) in
      Ans (CallDir (Id.L (trace_name), reds, []))
    else
      Inlining.inline_calldir_exp args fundef reg |> tj p reg mem tj_env
  | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) | IfGE (id_t, id_or_imm, t1, t2) as exp ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> reg.(int_of_id_t id)
      | C (n) -> Green (n)
    in
    begin match r1, r2 with
      | Green (n1), Green (n2) | LightGreen (n1), Green (n2) | Green (n1), LightGreen (n2) | LightGreen (n1), LightGreen (n2) ->
        if exp <=> (n1, n2)
        then tj p reg mem tj_env t1
        else tj p reg mem tj_env t2
      | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
        if exp <=> (n1, n2) then
          Ans (exp |%| (id_t, C (n2), tj p reg mem tj_env t1, Guard.restore_green reg t2))
        else
          Ans (exp |%| (id_t, C (n2), Guard.restore_green reg t1, tj p reg mem tj_env t2))
      | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
        let id_r2 = match id_or_imm with V (id) -> id | C (n) -> failwith "id_or_imm should be string" in
        if exp <=> (n1, n2) then
          Ans (exp |%| (id_r2, C (n1), tj p reg mem tj_env t1, Guard.restore_green reg t2))
        else
          Ans (exp |%| (id_r2, C (n1), Guard.restore_green reg t1, tj p reg mem tj_env t2))
      | Red (n1), Red (n2) ->
        if exp <=> (n1, n2) then
          Ans (exp |%| (id_t, id_or_imm, tj p reg mem tj_env t1, Guard.restore_green reg t2))
        else
          Ans (exp |%| (id_t, id_or_imm, Guard.restore_green reg t1, tj p reg mem tj_env t2))
    end
  | e ->
    begin match Jit_optimizer.run p e reg mem with
      | Specialized (v) -> Ans (Set (value_of v))
      | Not_specialized (e, v) -> Ans (e)
    end

let run_while p reg mem name reds index_pc merge_pc =
  let Prog (tbl, _, m) = p in
  let Jit_prep.Env (fdfs, ibody, reds) = Jit_prep.prep ~prog:p ~name:name ~red_args:reds in
  let p' = Prog (tbl, fdfs, m) in
  let trace = tj p' reg mem { index_pc = index_pc; merge_pc = merge_pc; trace_name = name } ibody in
  { name = Id.L (name); args = reds; fargs = []; body = trace; ret = Type.Int }
