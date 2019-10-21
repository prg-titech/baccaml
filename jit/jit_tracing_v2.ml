open Std

open Base
open Asm

open Jit_env
open Jit_util

open Printf

module Util = struct
  let rec find_by_inst inst t =
    match t with
    | Ans (IfEq (id_t, C (n), t1, t2)) ->
      if inst = n then t1
      else find_by_inst inst t2
    | Ans exp -> raise Not_found
    | Let (_, _, t) -> find_by_inst inst t

  let find_var var t =
    Asm.fv t
    |> List.find (fun v -> String.get_name v = String.get_name var)

  let filter ~reds args =
    List.filter (fun arg -> List.mem (String.get_name arg) reds) args

end

module JO = Jit_optimizer

let rec tj p reg mem env t =
  match t with
  | Ans (CallDir (id_l, argsr, fargsr)) ->
    (* let rec f argt argr =
     *   match argt, argr with
     *   | [], [] -> ()
     *   | hdt :: tlt, hdr :: tlr ->
     *     reg.(int_of_id_t hdt) <- reg.(int_of_id_t hdr);
     *     f tlt tlr
     *   | _ -> assert false in *)
    let { trace_name; index_pc; merge_pc; bytecode } = env in
    let pc = List.nth argsr index_pc |> int_of_id_t |> Array.get reg |> value_of in
    if pc = merge_pc then
      Ans (CallDir (Id.L trace_name, List.([nth argsr 0; nth argsr 1]), []))
    else
      let next_instr = bytecode.(pc) in
      Log.debug @@ sprintf "pc: %d, next instr: %d" pc next_instr;
      let { name; args= argst; fargs; body; ret } = Fundef.find_fuzzy p "interp" in
      let body = Util.find_by_inst next_instr body in
      Inlining.inline_fundef reg argsr { name; args= argst; fargs; body; ret }
      |> tj p reg mem env
  | Ans (exp) ->
    begin
      match exp with
      | IfEq _ | IfLE _ | IfGE _ | SIfEq _ | SIfLE _ | SIfGE _ -> tj_if p reg mem env exp
      | _ ->
        match JO.run p exp reg mem with
        | Specialized v -> Ans (Set (value_of v))
        | Not_specialized (e, v) -> Ans (e)
    end
  | Let ((dest, typ), CallDir (Id.L "min_caml_jit_merge_point", args, fargs), body) ->
    let pc = value_of @@ reg.(int_of_id_t @@ List.hd args) in
    Log.debug @@ sprintf "jit_merge_point: %d" pc;
    tj p reg mem env body
  | Let ((dest, typ), CallDir (Id.L "min_caml_method_entry", args, fargs), body) ->
    tj p reg mem env body
  | Let ((dest, typ), CallDir (Id.L "min_caml_can_enter_jit", args, fargs), body) ->
    let { trace_name; index_pc; merge_pc } = env in
    let pc = value_of @@ reg.(int_of_id_t @@ List.nth args index_pc) in
    Log.debug @@ sprintf "can_enter_jit: %d" pc;
    if pc = merge_pc
    then Ans (CallDir (Id.L trace_name, List.([nth args 0; nth args 1]), []))
    else tj p reg mem env body
  | Let ((dest, typ), CallDir (Id.L x, args, fargs), body) when String.starts_with x "min_caml" ->
     let { red_names } = env in
     Jit_guard.restore reg
       (Let ((dest, typ)
           , CallDir (Id.L x, Util.filter ~reds:red_names args, fargs)
           , (tj p reg mem env body)))
       args
  | Let ((dest, typ), e, body) ->
    begin match e with
    | IfEq _ | IfLE _ | IfGE _ | SIfEq _ | SIfLE _ | SIfGE _ ->
       Asm.concat (tj_if p reg mem env e) (dest, typ) (tj p reg mem env body)
    | St (id_t1, id_t2, id_or_imm, x) ->
       let srcv = reg.(int_of_id_t id_t1) in
       let destv = reg.(int_of_id_t id_t2) in
       let offsetv = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
       begin match (srcv, destv) with
       | Green n1, Red n2 ->
          reg.(int_of_id_t dest) <- Green 0;
          mem.(n1 + (n2 * x)) <- Green (int_of_id_t id_t1 |> Array.get reg |> value_of);
          (match offsetv with
           | Green n ->
              let id' = Id.gentmp Type.Int in
              let body' = tj p reg mem env body in
              Let ((id_t1, Type.Int), Set n1
                   , Let ((id', Type.Int), Set n
                          , Let ((dest, typ), St (id_t1, id_t2, C n, x), body')))
           | Red n ->
              let body' = tj p reg mem env body in
              Let ((id_t1, Type.Int), Set n1
                   , Let ((dest, typ), St (id_t1, id_t2, id_or_imm, x), body')))
       | _ -> body |> optimize_exp p reg mem env (dest, typ) e
       end
    | _ -> body |> optimize_exp p reg mem env (dest, typ) e
    end


and optimize_exp p reg mem env (dest, typ) exp body =
  match JO.run p exp reg mem with
  | Specialized v ->
    reg.(int_of_id_t dest) <- v;
    tj p reg mem env body
  | Not_specialized (e, v) ->
    reg.(int_of_id_t dest) <- v ;
    Let ((dest, typ), e, tj p reg mem env body)


and tj_if p reg mem env exp =
  let trace = tj p reg mem env in
  let guard = Jit_guard.TJ.create reg env.trace_name in
  match exp with
  | IfLE (id_t, id_or_imm, t1, t2) | SIfLE (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
    begin
      match r1, r2 with
      | Green n1, Green n2 ->
        if n1 <= n2 then trace t1
        else trace t2
      | Red n1, Green n2 ->
        if n1 <= n2 then
          Ans (IfLE (id_t, C (n2), trace t1, guard t2))
        else
          Ans (IfLE (id_t, C (n2), guard t1, trace t2))
      | Green n1, Red n2 ->
        let id_t2 = match id_or_imm with
            V (id) -> id
          | C _ -> failwith "un matched pattern."
        in
        if n1 <= n2 then
          Ans (IfGE (id_t2, C (n1), trace t2, guard t1))
        else
          Ans (IfGE (id_t2, C (n1), guard t2, trace t1))
      | Red n1, Red n2 ->
        if n1 <= n2 then
          Ans (IfLE (id_t, id_or_imm, trace t1, guard t2))
        else
          Ans (IfLE (id_t, id_or_imm, guard t1, trace t2))
    end
  | IfEq (id_t, id_or_imm, t1, t2) | SIfEq (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
    if String.get_name id_t = "mode" then begin
      Log.debug @@ Printf.sprintf "IfEq mode checking (%s, %s) ==> %d %d"
                     id_t (string_of_id_or_imm id_or_imm) (value_of r1) (value_of r2);
      Ans (IfEq (id_t, C (200), trace t1, guard t2))
    end else
      begin match r1, r2 with
        | Green n1, Green n2 ->
          if n1 = n2
          then tj p reg mem env t1
          else tj p reg mem env t2
        | Red n1, Green n2 ->
          if n1 = n2
          then Ans (IfEq (id_t, C (n2), trace t1, guard t2))
          else Ans (IfEq (id_t, C (n2), guard t1, trace t2))
        | Green n1, Red n2 ->
          let id_t2 = match id_or_imm with
              V (id) -> id
            | C _ -> failwith "un matched pattern."
          in
          if n1 = n2 then
            Ans (IfEq (id_t2, C (n1), trace t1, guard t2))
          else
            Ans (IfEq (id_t2, C (n1), guard t1, trace t2))
        | Red n1, Red n2 ->
          if n1 = n2 then
            Ans (IfEq (id_t, id_or_imm, trace t1, guard t2))
          else
            Ans (IfEq (id_t, id_or_imm, guard t1, trace t2))
      end
  | IfGE (id_t, id_or_imm, t1, t2) | SIfGE (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
    begin
      match r1, r2 with
      | Green n1, Green n2 ->
        if n1 >= n2 then
          tj p reg mem env t1
        else
          tj p reg mem env t2
      | Red n1, Green n2 ->
        if n1 >= n2 then
          Ans (IfGE (id_t, C (n2), trace t1, guard t2))
        else
          Ans (IfGE (id_t, C (n2), guard t1, trace t2))
      | Green n1, Red n2 ->
        let id_t2 = match id_or_imm with
            V (id) -> id
          | C _ -> failwith "un matched pattern."
        in
        if n1 >= n2 then
          Ans (IfLE (id_t2, C (n1), trace t1, guard t2))
        else
          Ans (IfLE (id_t2, C (n1), guard t1, trace t2))
      | Red n1, Red n2 ->
        if n1 >= n2 then
          Ans (IfGE (id_t, id_or_imm, trace t1, guard t2))
        else
          Ans (IfGE (id_t, id_or_imm, guard t1, trace t2))
    end


let run p reg mem ({ trace_name; red_names; merge_pc} as env) =
  Renaming.counter := !Id.counter;
  Log.debug @@ Printf.sprintf "staring trace (merge_pc: %d)" merge_pc;
  let (Prog (tbl, _, fundefs, m)) = p in
  let {body= ibody; args= iargs} = Fundef.find_fuzzy p "interp" in
  let trace = ibody |> tj p reg mem env in
  { name= Id.L trace_name ; fargs= []; body= trace; ret= Type.Int
  ; args= iargs |> List.filter (fun arg -> List.mem (String.get_name arg) red_names)
  }
