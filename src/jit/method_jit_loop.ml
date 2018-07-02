open Core
open Mincaml
open Asm
open Util
open Inlining
open Renaming
open Jit_config
open Jit_util
open Operands

let rec restore_args cont reg = function
    [] -> cont
  | hd :: tl ->
    if is_green reg.(int_of_id_t hd) then
      Let ((hd, Type.Int),
           Set (value_of reg.(int_of_id_t hd)),
           restore_args cont reg tl)
    else restore_args cont reg tl

let empty_fenv () = []

let extend_fenv func fenv = func :: fenv

let rec mj p reg mem fenv t =
  match t with
  | Ans (exp) ->
    mj_exp p reg mem fenv exp
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_start"), args, fargs), body) ->
    Logger.debug "min_caml_loop_start";
    let extended_fenv = extend_fenv body fenv in
    Ans (CallDir (Id.L ("test_loop_start"), args, fargs)), extended_fenv
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_end"), args, fargs), body) ->
    Logger.debug "min_caml_loop_end";
    let extended_fenv = extend_fenv body fenv in
    Ans (CallDir (Id.L ("test_loop_end"), args, fargs)), extended_fenv
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) ->
    let restored_call =
      restore_args
        (Let ((dest, typ), CallDir (id_l, args, fargs), Ans (Nop)))
        reg
        args
    in
    let t' = mj p reg mem fenv body in
    add_cont_proc (Id.gentmp Type.Int) restored_call (fst t'), snd t'
  | Let ((dest, typ), exp, body) ->
    begin match exp with
      | _ ->
        match Optimizer.optimize_exp p exp reg mem with
        | Specialized (v) ->
          reg.(int_of_id_t dest) <- v;
          mj p reg mem fenv body
        | Not_specialized (e, v) ->
          reg.(int_of_id_t dest) <- v;
          let t, x = mj p reg mem fenv body in
          Let ((dest, typ), e, t), x
    end

and mj_exp p reg mem fenv exp =
  match exp with
  | CallDir (Id.L ("min_caml_loop_end"), args, fargs) ->
    Logger.debug "min_caml_loop_end";
    Ans (CallDir (Id.L ("test_loop"), args, fargs)), fenv
  | CallDir (id_l, args, fargs) ->
    Logger.debug (Printf.sprintf "CallDir (%s)" (string_of_id_l id_l));
    let fundef = find_fundef p id_l in
    let t = Inlining.inline_calldir_exp args fundef reg in
    mj p reg mem fenv t
  | IfEq _ | IfGE _ | IfLE _ as exp ->
    mj_if p reg mem fenv exp
  | _ ->
    begin match Optimizer.optimize_exp p exp reg mem with
      | Specialized (v) ->
        let id = Id.gentmp Type.Int in
        Let ((id, Type.Int),
             Set (value_of v),
             Ans (Mov (id))), fenv
      | Not_specialized (e, v) -> Ans (e), fenv
    end

and mj_if p reg mem fenv exp =
  match exp with
  | IfGE (id_t, id_or_imm, t1, t2) | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) when (is_opcode id_t) ->
    Logger.debug (Printf.sprintf "If (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if exp |*| (r1, r2)
    then mj p reg mem fenv t1
    else mj p reg mem fenv t2
  | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) | IfGE (id_t, id_or_imm, t1, t2) ->
    Logger.debug (Printf.sprintf "If (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1, regt2 = Array.copy reg, Array.copy reg in
    let memt1, memt2 = Array.copy mem, Array.copy mem in
    let t1' = mj p regt1 memt1 fenv t1 in
    let t2' = mj p regt2 memt2 fenv t2 in
    begin match r1, r2 with
      | Green (n1), Green (n2)
      | LightGreen (n1), LightGreen (n2)
      | Green (n1), LightGreen (n2)
      | LightGreen (n1), Green (n2) ->
        if exp |*| (n1, n2) then t1' else t2'
      | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
        (* Ans (IfEq (id_t, C (n2), fst t1', fst t2')), snd t1' *)
        Ans (exp |%| (id_t, C (n2), fst t1', fst t2')), snd t1'
      | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
        let id_t2 = match id_or_imm with
            V (id) -> id
          | C (n) -> failwith "id_or_imm should be string"
        in
        Ans (exp |%|  (id_t2, C (n1), fst t1', fst t2')), snd t1'
      | Red (n1), Red (n2) ->
        Ans (exp |%| (id_t, id_or_imm, fst t1', fst t2')), snd t1'
    end
  | _ -> failwith "method_jit_if should accept conditional branches."

let run p reg mem t =
  let res, fenv' =  mj p reg mem [] t in
  res,
  List.map fenv' (fun t -> mj p reg mem [] t |> fst)
