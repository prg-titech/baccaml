open Std

open MinCaml
open Asm

open Jit_env
open Jit_util
open Jit_prof

type mj_env = {
    trace_name : string;
    red_names : string list;
    index_pc : int;
    merge_pc : int;
    function_pcs : int list;
    bytecode : int array;
  }

let create_mj_env ~trace_name ~red_names ~index_pc ~merge_pc ~function_pcs ~bytecode =
  { trace_name; red_names; index_pc; merge_pc; function_pcs; bytecode }

module Util : sig
  type args = string list
  val find_by_inst : inst:int -> t -> t
  val filter_by_names : reds:string list -> string list -> string list
  val find_call_dests : int array -> int -> int list
  val get_pc : reg -> string -> int

  val ( <=> ) : exp -> (int * int) -> bool
  val ( <|> ) : exp -> (Id.t * id_or_imm * t * t) -> exp
end = struct
  let call_inst, ret_inst = 7, 8

  type args = string list

  let rec find_by_inst ~inst t =
    match t with
    | Ans (IfEq (id_t, C (n), t1, t2)) ->
       if inst = n then t1
       else find_by_inst inst t2
    | Ans exp -> raise Not_found
    | Let (_, _, t) -> find_by_inst inst t

  let filter_by_names ~reds args =
    args |> List.filter (fun arg -> List.mem (String.get_name arg) reds)

  let find_call_dests bytecode merge_pc =
    let bc_with_i = Array.to_list bytecode |> List.mapi (fun i instr -> (i,instr)) in
    let ret_pc = bc_with_i |> List.find (fun (i,instr) -> instr = ret_inst && merge_pc < i) |> fst in
    let call_pcs = bc_with_i |> List.find_all (fun (i,instr) -> instr = call_inst && i < ret_pc) |> List.map fst in
    call_pcs |> List.map (fun call_pc -> List.assoc (call_pc + 1) bc_with_i) |> List.unique

  let get_pc reg arg =
    arg |> int_of_id_t |> Array.get reg |> value_of

  let (<=>) e (n1, n2) =
    match e with
    | SIfEq _ | IfEq _ -> n1 = n2
    | SIfLE _ | IfLE _ -> n1 <= n2
    | SIfGE _ | IfGE _ -> n1 >= n2
    | _ -> assert false

  let (<|>) e (id_t, id_or_imm, t1, t2) =
    match e with
    | SIfEq _ | IfEq _ -> IfEq (id_t, id_or_imm, t1, t2)
    | SIfLE _ | IfLE _ -> IfLE (id_t, id_or_imm, t1, t2)
    | SIfGE _ | IfGE _ -> IfGE (id_t, id_or_imm, t1, t2)
    | _ -> assert false
end

let rec mj p reg mem ({trace_name; red_names; index_pc; merge_pc; function_pcs; bytecode} as env) = function
  | Ans (exp) -> exp |> mj_exp p reg mem env
  | Let ((x, typ), CallDir (Id.L ("min_caml_jit_merge_point"), args, fargs), body) ->
     let pc = List.hd args |> int_of_id_t |> Array.get reg |> value_of in
     Log.debug ("jit_merge_point: " ^ string_of_int pc);
     mj p reg mem env body
  | Let ((x, typ), CallDir (Id.L ("min_caml_can_enter_jit"), args, fargs), body) ->
     let pc = List.nth args index_pc |> int_of_id_t |> Array.get reg |> value_of in
     Log.debug ("can_enter_jit: " ^ string_of_int pc);
     mj p reg mem env body
  | Let ((x, typ), CallDir (Id.L ("min_caml_mj_call"), args, fargs), body) ->
     let pc = Util.get_pc reg (List.nth args index_pc) in
     let reds = Util.filter_by_names red_names args in
     if pc = merge_pc then
       Let ((x, typ), CallDir (Id.L (trace_name), reds, fargs), mj p reg mem env body)
     else
       (try
          let trace_name = Method_prof.find pc in
          Let ((x, typ), CallDir (Id.L (trace_name), reds, fargs), mj p reg mem env body)
        with Not_found ->
          Let ((x, typ), CallDir (Id.L ("interp"), args, fargs), mj p reg mem env body)
          |> Jit_guard.restore reg ~args)
  | Let ((x, typ), CallDir (id_l, args, fargs), body) ->
     let reds = Util.filter_by_names ~reds:red_names args in
     Let ((x, typ), CallDir (id_l, reds, []), body |> mj p reg mem env)
     |> Jit_guard.restore reg ~args:args
  | Let ((x, typ), exp, body) ->
     match exp with
     | (IfEq _ | IfGE _ | IfLE _ | SIfEq _ | SIfLE _ | SIfGE _) ->
        let t' = mj_if p reg mem env exp in
        let k = mj p reg mem env body in
        Asm.concat t' (x, typ) k
     | St (id_t1, id_t2, id_or_imm, bitsize) ->
        let srcv = reg.(int_of_id_t id_t1) in
        let destv = reg.(int_of_id_t id_t2) in
        let offsetv = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
        begin match (srcv, destv) with
        | Green n1, Red n2 ->
           reg.(int_of_id_t id_t1) <- Green 0;
           mem.(n1 + (n2 * bitsize)) <- Green (reg.(int_of_id_t id_t1) |> value_of) ;
           begin match offsetv with
           | Green n ->
              let id' = Id.gentmp Type.Int in
              Let ( (id_t1, Type.Int)
                  , Set n1
                  , Let ( (id', Type.Int)
                        , Set n
                        , Let ((x, typ), St (id_t1, id_t2, C n, bitsize)
                               , mj p reg mem env body)))
           | Red n ->
              Let ( (id_t1, Type.Int)
                  , Set n1
                  , Let ( (x, typ)
                        , St (id_t1, id_t2, id_or_imm, bitsize)
                        , mj p reg mem env body) )
           end
        | _ -> optimize_exp p reg mem (x, typ) env exp body
        end
     | _ -> optimize_exp p reg mem (x, typ) env exp body

and optimize_exp p reg mem (x, typ) env exp body =
  match Jit_optimizer.run p exp reg mem with
  | Specialized (v) ->
     reg.(int_of_id_t x) <- v;
     mj p reg mem env body
  | Not_specialized (e, v) ->
     reg.(int_of_id_t x) <- v;
     Let ((x, typ), e, mj p reg mem env body)

and mj_exp p reg mem ({index_pc; merge_pc; bytecode} as env) = function
  | CallDir (id_l, argsr, fargs) ->
     (* let rec f argt argr =
      *   match argt, argr with
      *   | [], [] -> ()
      *   | hdt :: tlt, hdr :: tlr ->
      *     reg.(int_of_id_t hdt) <- reg.(int_of_id_t hdr);
      *     f tlt tlr
      *   | _ -> assert false in *)
     let pc = List.nth argsr index_pc |> int_of_id_t |> Array.get reg |> value_of in
     let next_instr = bytecode.(pc) in
     let { name; args= argst; fargs; body; ret } = Fundef.find_fuzzy p "interp" in
     let t = Util.find_by_inst next_instr body in
     (* debug *)
     (* print_string (string_of_int pc ^ ", ");
      * Asm.print_t t; print_newline (); *)
     (* if dsable renaming *)
     (* f argst argsr; t |> mj p reg mem env *)
     { name; args= argst; fargs; body= t; ret }
     |> Inlining.inline_fundef reg argsr
     |> mj p reg mem env
  | IfEq _ | IfLE _ | IfGE _ | SIfEq _ | SIfGE _ | SIfLE _ as exp -> exp |> mj_if p reg mem env
  | exp ->
     match Jit_optimizer.run p exp reg mem with
     | Specialized v ->
        let id = Id.gentmp Type.Int in
        Let ((id, Type.Int), Set (value_of v), Ans (Mov id))
     | Not_specialized (e, v) -> Ans e

and mj_if p reg mem ({index_pc; merge_pc; bytecode} as env) = function
  | IfEq (id_t, id_or_imm, t1, t2)
  | IfLE (id_t, id_or_imm, t1, t2)
  | IfGE (id_t, id_or_imm, t1, t2) as exp ->
     if String.get_name id_t = "instr" then
       let pc = reg.(int_of_id_t id_t) |> value_of in
       Log.debug ("instr: " ^ string_of_int pc);
       let { body } = Fundef.find_fuzzy p "interp" in
       let t = Util.find_by_inst pc body in
       t |> mj p reg mem env
     else if String.get_name id_t = "mode" then
       let open Jit_guard in
       let guard_code = TJ.create reg env.trace_name t1 in
       Ans (IfEq (id_t, id_or_imm, guard_code, t2))
     else
       (Asm.print_exp exp; print_newline (); assert false)
  | SIfEq (id_t, id_or_imm, t1, t2)
  | SIfGE (id_t, id_or_imm, t1, t2)
  | SIfLE (id_t, id_or_imm, t1, t2) as exp ->
     let reg1 = Array.copy reg in
     let reg2 = Array.copy reg in
     let mem1 = Array.copy mem in
     let mem2 = Array.copy mem in
     let r1 = reg.(int_of_id_t id_t) in
     let r2 = reg.(int_of_id_or_imm id_or_imm) in
     let open Util in
     begin
       match r1, r2 with
       | Green n1, Green n2 ->
          if exp <=> (n1, n2)
          then t1 |> mj p reg mem env
          else t2 |> mj p reg mem env
       | Red n1, Green n2 ->
          let t1' = mj p reg1 mem1 env t1 in
          let t2' = mj p reg2 mem2 env t2 in
          Ans (exp <|> (id_t, C (n2), t1', t2'))
       | Green n1, Red n2 ->
          let t1' = mj p reg1 mem1 env t1 in
          let t2' = mj p reg2 mem2 env t2 in
          let id_t' = match id_or_imm with V id -> id | C _ -> assert false in
          Ans (exp <|> (id_t', C (n1), t2', t1'))
       | Red n1, Red n2 ->
          let t1' = mj p reg1 mem1 env t1 in
          let t2' = mj p reg2 mem2 env t2 in
          Ans (exp <|> (id_t, id_or_imm, t1', t2'))
     end


let run prog reg mem ({trace_name; red_names; index_pc; merge_pc; bytecode} : env) =
  Renaming.counter := !Id.counter;
  let { args; body } = Fundef.find_fuzzy prog "interp" in
  let reds = args |> List.find_all (fun x -> List.mem (String.get_name x) red_names) in
  let env = {trace_name; red_names; index_pc; merge_pc; function_pcs=[merge_pc]; bytecode} in
  let trace = mj prog reg mem env body in
  Fundef.create_fundef ~name:(Id.L env.trace_name) ~args:reds ~fargs:[] ~body:trace ~ret:(Type.Int)


let run_multi p reg mem ({trace_name; red_names; index_pc; merge_pc; bytecode} : env) =
  Renaming.counter := !Id.counter;
  let call_dests = Util.find_call_dests bytecode merge_pc in
  let merge_pcs = call_dests @ [merge_pc] |> List.unique in
  let { args } = Fundef.find_fuzzy p "interp" in
  let compiled =
    merge_pcs
    |> List.map (fun pc ->
      let trace_name = Trace_name.gen_str `Meta_method in
      Method_prof.register (pc, trace_name);
      pc, trace_name)
    |> List.map (fun (pc,trace_name) ->
      Log.debug @@ Printf.sprintf "method jit: pc %d, name %s" pc trace_name;
      let pc_id = args |> List.find (fun arg -> String.get_name arg = "pc") in
      let new_env = create_env ~trace_name:trace_name ~red_names:red_names
          ~index_pc:index_pc ~merge_pc:pc ~bytecode:bytecode in
      reg.(int_of_id_t pc_id) <- Green (pc);
      pc, run p reg mem new_env)
  in
  `Mj_result (List.assoc merge_pc compiled, List.(remove_assoc merge_pc compiled |> map snd))
