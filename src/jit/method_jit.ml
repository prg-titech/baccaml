open Core
open Mincaml
open Asm
open Util
open Inlining
open Jit_config
open Jit_util
open Renaming

exception Method_jit_failed of string

let loop_start_l = Id.L ("min_caml_loop_start")
let loop_end_l = Id.L ("min_caml_loop_end")
let loop_start_count = ref 0

let rec add_cont_proc id_t instr body =
  let rec go id_t instr body = match instr with
    | Let (a, Nop, t) -> go id_t t body
    | Let (a, e, t) -> Let (a, e, go id_t t body)
    | Ans e -> Let ((id_t, Type.Int), e, body)
  in go id_t instr body

let rec find_loop_t name = function
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) when id_l = loop_end_l ->
    Ans (CallDir (Id.L (name), args, fargs))
  | Let ((dest, typ), exp, body) -> Let ((dest, typ), exp, find_loop_t name body)
  | Ans (exp) -> find_loop_exp name exp

and find_loop_exp name = function
  | IfEq (id_t, id_or_imm, t1, t2)
  | IfLE (id_t, id_or_imm, t1, t2)
  | IfGE (id_t, id_or_imm, t1, t2) ->
    begin match find_loop_t name t1 with
      | t -> t
      | exception Not_found ->
        match find_loop_t name t2 with
        | t -> t
        | exception Not_found -> raise Not_found
    end
  | CallDir (loop_end_l, args, fargs) ->
    Ans (CallDir (Id.L (name), args, fargs))
  | _ -> raise Not_found

let rec find_loop name = function
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) when id_l = loop_start_l ->
    { name = Id.L (name); args = args; fargs = fargs; body = find_loop_t name body; ret = Type.Int }    
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) when id_l = loop_end_l ->
    raise Not_found
  | Let ((dest, typ), exp, body) ->
    find_loop name body
  | Ans (exp) ->
    raise Not_found

let rec find_nonloop_t name = function
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) when id_l = loop_start_l->
    find_nonloop_t name body      
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) when id_l = loop_end_l ->
    raise Not_found
  | Let ((dest, typ), exp, body) ->
    Let ((dest, typ), exp, find_nonloop_t name body)
  | Ans (exp) ->
    try find_nonloop_exp name exp with
    | Not_found -> Ans(Nop)

and find_nonloop_exp name = function
  | IfEq (id_t, id_or_imm, t1, t2) ->
    begin match find_loop name t1 with
      | _ ->
        Ans (IfEq (id_t, id_or_imm,
                   t1,
                   Ans (CallDir (Id.L (name), [id_t], []))))
      | exception Not_found ->
        Ans (IfEq (id_t, id_or_imm,
                   Ans (CallDir (Id.L (name), [id_t], [])),
                   t2))
    end
  | IfLE (id_t, id_or_imm, t1, t2) ->
    begin match find_loop name t1 with
      | _ ->
        Ans (IfLE (id_t, id_or_imm,
                   t1,
                   Ans (CallDir (Id.L (name), [id_t], []))))
      | exception Not_found ->
        Ans (IfLE (id_t, id_or_imm,
                   Ans (CallDir (Id.L (name), [id_t], [])),
                   t2))
    end
  | IfGE (id_t, id_or_imm, t1, t2) ->
    begin match find_loop name t1 with
      | _ ->
        Ans (IfGE (id_t, id_or_imm,
                   t1,
                   Ans (CallDir (Id.L (name), [id_t], []))))
      | exception Not_found ->
        Ans (IfGE (id_t, id_or_imm,
                   Ans (CallDir (Id.L (name), [id_t], [])),
                   t2))
    end
  | exp -> Ans (exp)


let find_nonloop name ({ name = name; args = args; fargs = fargs; body = body; ret = ret }) =
  let body' = find_nonloop_t "test_loop_fun" body in
  { name = name; args = args; fargs = fargs; body = body'; ret = ret }

let rec method_jit p instr reg mem jargs =
  match instr with
  | Ans (exp) -> method_jit_exp p exp reg mem jargs
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_start"), args, fargs), body) ->
    Logger.debug "min_caml_loop_start";
    Let ((dest, typ), CallDir (Id.L ("min_caml_loop_start"), args, fargs),
            method_jit p body reg mem jargs)
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_end"), args, fargs), body) ->
    Logger.debug "min_caml_loop_end";
    Ans (CallDir (Id.L "min_caml_loop_end", args, fargs))
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


let create_mj_reds trace_args (Prog (_, fundefs, _)) =
  let interp = List.find_exn ~f:begin fun { name = Id.L (x) } ->
      (String.split ~on:'.' x |> List.hd_exn) = "interp"
    end fundefs      
  in
  let { args } = interp in
  List.filter ~f:begin fun arg ->
    let arg_name = List.hd_exn (String.split ~on:'.' arg) in
    List.exists ~f:(fun trace_arg -> trace_arg = arg_name) trace_args
  end args

let create_mj_args name trace_args p =
  Method_jit_args (
    { method_name = name;
      reds = create_mj_reds trace_args p;
      method_start = 0;
      method_end = 0;
      pc_place = 1;
      loop_headers = [];
      backedge_pcs = []
    }
  )

let prep p t jit_args =
  let t' = Simm.t t |> Trim.trim_jmp |> Trim.trim_jit_dispatcher in
  Emit_virtual.to_string_t t' |> print_endline;
  let jit_args' = match jit_args with
      Tracing_jit_args v -> assert false
    | Method_jit_args m -> m
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
      let fundefs' = List.map fundefs (fun fundef ->
          let Id.L (x) = fundef.name in
          match String.split ~on:'.' x |> List.hd with
          | Some name' when name' = "interp" ->
            let { name; args; fargs; ret } = fundef in
            { name = name; args = args; fargs = fargs; body = interp_body; ret = ret }
          | _ -> fundef)
      in
      fundefs', interp_body, jit_args'
    | _ ->
      raise @@
      Method_jit_failed
        "missing jit_dispatch. please add jit_dispatch ... at the top of your interpreter."
  end


let prep' ~prog:p ~name:n ~red_args:reds =
  let Prog (table, fundefs, main) = p in
  let { body } = List.find_exn ~f:(fun { name = Id.L (x) } ->
      String.split ~on:'.' x |> List.hd_exn |> contains "interp"
    ) fundefs
  in
  let mj_args = create_mj_args n reds p in
  prep p body mj_args


let exec p t reg mem jit_args =
  let Prog (table, _, main) = p in
  let (fundefs', interp_body, jit_args') = prep' ~prog:p ~name:"min_caml_test_trace" ~red_args:["bytecode"; "a"] in
  let res = (method_jit (Prog (table, fundefs', main)) interp_body reg mem jit_args') in
  Method_success (
    { name = Id.L ("min_caml_test_trace")
    ; args = jit_args'.reds
    ; fargs = []
    ; body = res
    ; ret = Type.Int })
