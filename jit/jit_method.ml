open Std

open Base
open Asm

open Jit_env
open Jit_util

module Util = struct
  let call_inst = 7

  let ret_inst = 8

  let rec find_by_inst inst t =
    match t with
    | Ans (IfEq (id_t, C (n), t1, t2)) ->
       if inst = n then t1
       else find_by_inst inst t2
    | Ans exp -> raise Not_found
    | Let (_, _, t) -> find_by_inst inst t

  let filter_by_names ~red_names args =
    args |> List.filter (fun arg -> List.mem (String.get_name arg) red_names)

  let find_entry_all bytecode =
    (* TOOD: specify externally *)
    List.mapi (fun i x -> (i,x)) bytecode
    |> List.find_all (fun (i,x) -> x = 14)
    |> List.map fst

  let find_call_within bytecode merge_pc =
    let bytecode = Array.mapi (fun i x -> (i,x)) bytecode |> Array.to_list in
    let entry_pc, _ = bytecode |> List.find (fun (i,x) -> i = merge_pc)
    and ret_pc, _ = bytecode |> List.find (fun (i,x) -> x = ret_inst) in
    bytecode
    |> List.filter (fun (i,x) -> entry_pc <= i && i <= ret_pc)
    |> List.filter (fun (i,x) -> x = call_inst)

  let find_call_dest (bytecode : int array) (call_pcs : (int * int) list) =
    call_pcs |> List.map (fun (call_pc,_) -> Array.get bytecode (call_pc + 1))


  let _ =
    let bytecode = [|1;2;7;1;1;2;3;14;9;1;9;2;5;3;7;1;1;5;3;7;21;2;8;|]
    and merge_pc = 7 in
    assert (find_call_within bytecode merge_pc = [(14,7); (19,7)]);
    assert (find_call_within bytecode merge_pc |> find_call_dest bytecode = [1;21])


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

let rec mj p reg mem ({trace_name; index_pc; merge_pc; red_names; bytecode} as env : env) = function
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
     Let ((x, typ), CallDir (Id.L (trace_name), Util.filter_by_names red_names args, fargs), mj p reg mem env body)
  | Let ((x, typ), CallDir (id_l, args, fargs), body) ->
     Let ((x, typ), CallDir (id_l, args, fargs), body |> mj p reg mem env)
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

and mj_exp (p : prog) (reg : reg) (mem : mem) ({index_pc; merge_pc; bytecode} as env : env) : exp -> t =
  function
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

and mj_if (p : prog) (reg : reg) (mem : mem) ({index_pc; merge_pc; bytecode} as env : env) : exp -> t =
  function
  | IfEq (id_t, id_or_imm, t1, t2)
  | IfLE (id_t, id_or_imm, t1, t2)
  | IfGE (id_t, id_or_imm, t1, t2) ->
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
       assert false
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


let run prog reg mem ({trace_name; red_names; index_pc= x; merge_pc= y} as env) =
  Renaming.counter := !Id.counter;
  let { args; body } = Fundef.find_fuzzy prog "interp" in
  let trace = mj prog reg mem env body in
  { name= Id.L env.trace_name; fargs= []; body= trace; ret= Type.Int
  ; args= args |> List.filter (fun arg -> List.mem (String.get_name arg) red_names)
  }
