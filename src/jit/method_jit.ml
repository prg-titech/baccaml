open Core
open Mincaml
open Asm
open Util
open Inlining
open Jit_config
open Jit_util
open Renaming

module TJ = Tracing_jit

exception Method_jit_failed of string

let rec add_cont_proc id_t instr body =
  let rec go id_t instr body = match instr with
    | Let (a, Nop, t) -> go id_t t body
    | Let (a, e, t) -> Let (a, e, go id_t t body)
    | Ans e -> Let ((id_t, Type.Int), e, body)
  in go id_t instr body

let rec method_jit p instr reg mem jargs =
  match instr with
  | Ans (exp) -> method_jit_exp p exp reg mem jargs
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_start"), _, _), body) ->
    Logger.debug "min_caml_loop_start";
    Let ((dest, typ), CallDir (Id.L ("min_caml_loop_func_start"), [], []),
         method_jit p body reg mem jargs)
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_end"), _, _), body) ->
    Logger.debug "min_caml_loop_end";
    Ans (CallDir (Id.L ("min_caml_loop_func_end"), [], []))
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) ->
    let rec restore_args cont = function
        [] -> cont
      | hd :: tl ->
        if is_green reg.(int_of_id_t hd) then
          Let ((hd, Type.Int),
               Set (value_of reg.(int_of_id_t hd)),
               restore_args cont tl)
        else restore_args cont tl
    in
    let restored_fcall =
      restore_args
        (Let ((dest, typ), CallDir (id_l, args, fargs), Ans (Nop)))
        args
    in
    let t' = method_jit p body reg mem jargs in
    add_cont_proc (Id.gentmp Type.Unit) restored_fcall t'
  | Let ((dest, typ), exp, body) ->
    begin match Optimizer.optimize_exp p exp reg mem with
      | Specialized (v) ->
        reg.(int_of_id_t dest) <- v;
        method_jit p body reg mem jargs
      | Not_specialized (e, v) ->
        reg.(int_of_id_t dest) <- v;
        let t = method_jit p body reg mem jargs in
        Let ((dest, typ), e, t)
    end

and method_jit_exp p e reg mem jargs =
  match e with
  | CallDir (id_l, argsr, _) ->
    Logger.debug (Printf.sprintf "CallDir (%s)" (string_of_id_l id_l));
    let fundef = find_fundef p id_l in
    let t = Inlining.inline_calldir_exp argsr fundef reg in
    method_jit p t reg mem jargs
  | IfEq _ | IfGE _ | IfLE _ ->
    method_jit_if p e reg mem jargs
  | _ ->
    begin match Optimizer.optimize_exp p e reg mem with
      | Specialized (v) ->
        let id = Id.gentmp Type.Int in
        Let ((id, Type.Int),
             Set (value_of v),
             Ans (Mov (id)))
      | Not_specialized (e, v) -> Ans (e)
    end

and method_jit_if p e reg mem jargs =
  match e with
  | IfLE (id_t, id_or_imm, t1, t2) when (is_opcode id_t) ->
    Logger.debug (Printf.sprintf "IfLE (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 <= r2
    then method_jit p t1 reg mem jargs
    else method_jit p t2 reg mem jargs
  | IfEq (id_t, id_or_imm, t1, t2) when (is_opcode id_t) ->
    Logger.debug (Printf.sprintf "IfEq (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 = r2
    then method_jit p t1 reg mem jargs
    else method_jit p t2 reg mem jargs
  | IfGE (id_t, id_or_imm, t1, t2) when (is_opcode id_t) ->
    Logger.debug (Printf.sprintf "IfGE (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 >= r2
    then method_jit p t1 reg mem jargs
    else method_jit p t2 reg mem jargs
  | IfEq (id_t, id_or_imm, t1, t2) ->
    Logger.debug (Printf.sprintf "IfEq (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1 = Array.copy reg in
    let regt2 = Array.copy reg in
    let memt1 = Array.copy mem in
    let memt2 = Array.copy mem in
    let t1' = method_jit p t1 regt1 memt1 jargs in
    let t2' = method_jit p t2 regt2 memt2 jargs in
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
    Logger.debug (Printf.sprintf "IfLE (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1 = Array.copy reg in
    let regt2 = Array.copy reg in
    let memt1 = Array.copy mem in
    let memt2 = Array.copy mem in
    let t1' = method_jit p t1 regt1 memt1 jargs in
    let t2' = method_jit p t2 regt2 memt2 jargs in
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
    Logger.debug (Printf.sprintf "IfGE (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1 = Array.copy reg in
    let regt2 = Array.copy reg in
    let memt1 = Array.copy mem in
    let memt2 = Array.copy mem in
    let t1' = method_jit p t1 regt1 memt1 jargs in
    let t2' = method_jit p t2 regt2 memt2 jargs in
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

let exec p t reg mem jit_args =
  let t' = Simm.t t |> Trim.trim_jmp in
  Emit_virtual.to_string_t t' |> print_endline;
  let jit_args' = match jit_args with
      Tracing_jit_args v -> assert false
    | Method_jit_args m -> m
  in
  begin match t' with
    | Let (_, Set (_),
           Let (_,  IfEq (x, y, _, _),
           Let (_, CallDir (Id.L (_), args, fargs),
                interp_body)))
    | Let (_,  IfEq (x, y, _, _),
           Let (_, CallDir (Id.L (_), args, fargs),
                interp_body)) ->
      let Prog (table, fundefs, main) = p in
      let fundefs' = List.map fundefs (fun fundef ->
          let Id.L (x) = fundef.name in
          match String.split ~on:'.' x |> List.hd with
          | Some name' when name' = "interp" ->
            let { name; args; fargs; ret } = fundef in
            { name = name; args = args; fargs = fargs; body = interp_body; ret = ret }
          | _ -> fundef)
      in
      method_jit (Prog (table, fundefs', main)) interp_body reg mem jit_args', jit_args'
    | Ans _ | Let _ ->
      raise @@
      Method_jit_failed
        "missing jit_dispatch. please add jit_dispatch ... at the top of your interpreter."
  end
  |> fun (res, args) ->
  Method_success (
    { name = Id.L ("min_caml_test_trace")
    ; args = args.reds
    ; fargs = []
    ; body = res
    ; ret = Type.Int })
