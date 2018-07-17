open MinCaml
open Asm
open Util
open Inlining
open Renaming
open Jit_config
open Jit_util
open Operands

module M = Map.Make(String)

type value = string list * t

let keys m = M.bindings m |> List.map fst

let empty_fenv () = M.empty

let extend_fenv name args func fenv = M.add name (args, func) fenv

let gen_fname id = Id.genid id

let rec restore_args cont reg = function
    [] -> cont
  | hd :: tl ->
    if is_green reg.(int_of_id_t hd) then
      Let ((hd, Type.Int),
           Set (value_of reg.(int_of_id_t hd)),
           restore_args cont reg tl)
    else restore_args cont reg tl

let rec mj p reg mem fenv name t =
  match t with
  | Ans (exp) ->
    mj_exp p reg mem fenv name exp
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_start"), args, fargs), body) ->
    Logger.debug "min_caml_loop_start";
    let fname = gen_fname "loop_start" in
    let extended_fenv = extend_fenv fname args body fenv in
    Ans (CallDir (Id.L (fname), args, fargs)), extended_fenv
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_end"), args, fargs), body) ->
    Logger.debug "min_caml_loop_end";
    Ans (CallDir (Id.L (name), args, fargs)), M.empty
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) ->
    let restored_call =
      restore_args
        (Let ((dest, typ), CallDir (id_l, args, fargs), Ans (Nop)))
        reg
        args
    in
    let t' = mj p reg mem fenv name body in
    connect (Id.gentmp Type.Int) restored_call (fst t'), snd t'
  | Let ((dest, typ), exp, body) ->
    begin match exp with
      | IfEq _ | IfGE _ | IfLE _ ->
        let t' = mj_if p reg mem fenv name exp in
        let k = mj p reg mem fenv name body in
        connect dest (fst t') (fst k), snd k
      | _ ->
        match Optimizer.run p exp reg mem with
        | Specialized (v) ->
          reg.(int_of_id_t dest) <- v;
          mj p reg mem fenv name body
        | Not_specialized (e, v) ->
          reg.(int_of_id_t dest) <- v;
          let t, x = mj p reg mem fenv name body in
          Let ((dest, typ), e, t), x
    end

and mj_exp p reg mem fenv name exp =
  match exp with
  | CallDir (id_l, args, fargs) ->
    Logger.debug (Printf.sprintf "CallDir (%s)" (string_of_id_l id_l));
    let fundef = find_fundef p id_l in
    let t = Inlining.inline_calldir_exp args fundef reg in
    mj p reg mem fenv name t
  | IfEq _ | IfGE _ | IfLE _ as exp ->
    mj_if p reg mem fenv name exp
  | _ ->
    begin match Optimizer.run p exp reg mem with
      | Specialized (v) ->
        let id = Id.gentmp Type.Int in
        Let ((id, Type.Int),
             Set (value_of v),
             Ans (Mov (id))), M.empty
      | Not_specialized (e, v) -> Ans (e), M.empty
    end

and mj_if p reg mem fenv name exp =
  match exp with
  | IfGE (id_t, id_or_imm, t1, t2) | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) when (is_opcode id_t) ->
    Logger.debug (Printf.sprintf "If (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if exp |*| (r1, r2)
    then mj p reg mem fenv name t1
    else mj p reg mem fenv name t2
  | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) | IfGE (id_t, id_or_imm, t1, t2) ->
    Logger.debug (Printf.sprintf "If (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1, regt2 = Array.copy reg, Array.copy reg in
    let memt1, memt2 = Array.copy mem, Array.copy mem in
    let t1' = mj p regt1 memt1 fenv name t1 in
    let t2' = mj p regt2 memt2 fenv name t2 in
    begin match r1, r2 with
      | Green (n1), Green (n2)
      | LightGreen (n1), LightGreen (n2)
      | Green (n1), LightGreen (n2)
      | LightGreen (n1), Green (n2) ->
        if exp |*| (n1, n2) then t1' else t2'
      | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
        Ans (exp |%| (id_t, C (n2), fst t1', fst t2')), snd t1'
      | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
        let id_t2 = match id_or_imm with
            V (id) -> id
          | C (n) -> failwith "id_or_imm should be string"
        in
        Ans (exp |%|  (id_t2, C (n1), fst t1', fst t2')), snd t2'
      | Red (n1), Red (n2) ->
        Ans (exp |%| (id_t, id_or_imm, fst t1', fst t2')), snd t2'
    end
  | _ -> failwith "method_jit_if should accept conditional branches."


let create_mj_reds trace_args (Prog (_, fundefs, _)) =
  let interp =
    List.find begin fun { name = Id.L (x) } ->
      (String.split_on_char '.' x |> List.hd) = "interp"
    end fundefs
  in
  let { args } = interp in
  List.filter begin fun arg ->
    let arg_name = List.hd (String.split_on_char '.' arg) in
    List.exists (fun trace_arg -> trace_arg = arg_name) trace_args
  end args


let prep' p t reds =
  let t' =
    Simm.t t
    |> Trim.trim_jmp
    |> Trim.trim_jit_dispatcher
  in
  begin match t' with
    | Let (_, Set (_),
           Let (_,  IfEq (_, _, _, _),
                Let (_, CallDir (Id.L (_), args, fargs),
                     interp_body)))
    | Let (_,  IfEq (_, _, _, _),
           Let (_, CallDir (Id.L (_), args, fargs),
                interp_body))
    | Ans (IfEq (_, _, Ans (CallDir (Id.L (_), args, fargs)),
                 interp_body)) ->
      let Prog (table, fundefs, main) = p in
      let fundefs' =
        List.map begin fun fundef ->
            let Id.L (x) = fundef.name in
            match String.split_on_char '.' x |> List.hd with
            | name' when name' = "interp" ->
              let { name; args; fargs; ret } = fundef in
              { name = name; args = args; fargs = fargs; body = interp_body; ret = ret }
            | _ -> fundef
        end fundefs
      in
      fundefs', interp_body, reds
    | _ ->
      failwith
        "missing jit_dispatch. please add jit_dispatch ... at the top of your interpreter."
  end

let prep ~prog:p ~name:n ~red_args:reds =
  let Prog (table, fundefs, main) = p in
  let { body } = List.find begin fun { name = Id.L (x) } ->
      String.split_on_char '.' x |> List.hd |> contains "interp"
    end fundefs
  in
  prep' p body (create_mj_reds reds p)

let run p reg mem name reds =
  let Prog (tbl, _, m) = p in
  let (fdfs, ibody, reds) = prep ~prog:p ~name:name ~red_args:reds in
  let p' = Prog (tbl, fdfs, m) in
  let fenv = M.empty in
  let res, fenv' =  mj p' reg mem fenv name ibody in
  { name = Id.L (name); args = reds; fargs = []; body = res; ret = Type.Int } ::
  List.map begin fun (n, (ag, t)) ->
    let res, fenv' = mj p' reg mem fenv' n t in
    { name = Id.L (n); args = ag; fargs = []; body = res; ret = Type.Int }
  end (M.bindings fenv')

let run_while p reg mem name reds =
  let Prog (tbl, _, m) = p in
  let (fdfs, ibody, reds) = prep ~prog:p ~name:name ~red_args:reds in
  let p' = Prog (tbl, fdfs, m) in
  let rec loop p reg mem fenv name args t =
    let res, fenv' = mj p reg mem fenv name t in
    match M.choose_opt fenv' with
    | Some (n, (ag, t')) ->
      (res, name, reds) :: (loop p reg mem M.empty n ag t')
    | None ->
      (res, name, args) :: []
  in
  let loops = loop p' reg mem M.empty name reds ibody in
  List.map begin fun (body, name, args) ->
    { name = Id.L (name); args = args; fargs = []; body = body; ret = Type.Int }
  end loops
