open Mincaml
open Asm
open Core
open Inlining
open Jit_config
open Renaming

let find_pc argsr method_jit_args =
  match List.nth argsr (method_jit_args.pc_place) with
  | Some (s) -> int_of_id_t s
  | None -> failwith "find_pc is failed."

let rec find_fundef prog name =
  let Asm.Prog (_, fundefs, _) = prog in
  match List.find fundefs ~f:(fun fundef -> fundef.name = name) with
  | Some (body) -> body
  | None -> failwith "find_fundef is failed"

let jit_value_of_id_t reg id_t = reg.(int_of_id_t id_t)

let jit_value_of_id_or_imm reg = function
  | V (id) -> reg.(int_of_id_t id)
  | C (n) -> Green (n)

let name_of id =
  match List.hd (String.split id ~on:'.') with
  | Some (v) -> v
  | None -> id

let is_opcode id =
  String.equal (name_of id) "instr"

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

let string_of_id_l id_l = match id_l with
  | Id.L (x) -> x

let rec add_cont_proc id_t instr body =
  let rec go id_t instr body = match instr with
    | Let (a, Nop, t) -> go id_t t body
    | Let (a, e, t) -> Let (a, e, go id_t t body)
    | Ans e -> Let ((id_t, Type.Int), e, body)
  in go id_t instr body

let rec method_jit p instr reg mem method_jit_args =
  match instr with
  | Ans (exp) -> method_jit_ans p exp reg mem method_jit_args
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
    let t' = method_jit p body reg mem method_jit_args in
    add_cont_proc (Id.gentmp Type.Unit) restored_fcall t'
  | Let ((dest, typ), exp, body) ->
    begin match Optimizer.optimize_exp p exp reg mem with
      | Specialized (v) ->
        reg.(int_of_id_t dest) <- v;
        method_jit p body reg mem method_jit_args
      | Not_specialized (e, v) ->
        reg.(int_of_id_t dest) <- v;
        let t = method_jit p body reg mem method_jit_args in
        Let ((dest, typ), e, t)
    end

and method_jit_ans p e reg mem method_jit_args = match e with
  | CallDir (id_l, argsr, _) ->
    begin
      if contains (string_of_id_l id_l) "min_caml"
      then Ans (e)
      else
        let fundef = find_fundef p id_l in
        let pc = value_of reg.(find_pc argsr method_jit_args) in
        begin match (pc = (method_jit_args.method_end)) with
          | true ->
            Ans (Nop)
          | false ->
            let t' = Inlining.inline_calldir_exp argsr fundef reg in
            method_jit p t' reg mem method_jit_args
        end
    end
  | IfLE (id_t, id_or_imm, t1, t2) when (is_opcode id_t) ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 <= r2
    then method_jit p t1 reg mem method_jit_args
    else method_jit p t2 reg mem method_jit_args
  | IfEq (id_t, id_or_imm, t1, t2) when (is_opcode id_t) ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 = r2
    then method_jit p t1 reg mem method_jit_args
    else method_jit p t2 reg mem method_jit_args
  | IfGE (id_t, id_or_imm, t1, t2) when (is_opcode id_t) ->
    let r1 = value_of reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> value_of reg.(int_of_id_t id)
      | C (n) -> n
    in
    if r1 >= r2
    then method_jit p t1 reg mem method_jit_args
    else method_jit p t2 reg mem method_jit_args
  | IfEq (id_t, id_or_imm, t1, t2) ->
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1 = Array.copy reg in
    let regt2 = Array.copy reg in
    let memt1 = Array.copy mem in
    let memt2 = Array.copy mem in
    let t1' = method_jit p t1 regt1 memt1 method_jit_args in
    let t2' = method_jit p t2 regt2 memt2 method_jit_args in
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
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1 = Array.copy reg in
    let regt2 = Array.copy reg in
    let memt1 = Array.copy mem in
    let memt2 = Array.copy mem in
    let t1' = method_jit p t1 regt1 memt1 method_jit_args in
    let t2' = method_jit p t2 regt2 memt2 method_jit_args in
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
    let r1 = jit_value_of_id_t reg id_t in
    let r2 = jit_value_of_id_or_imm reg id_or_imm in
    let regt1 = Array.copy reg in
    let regt2 = Array.copy reg in
    let memt1 = Array.copy mem in
    let memt2 = Array.copy mem in
    let t1' = method_jit p t1 regt1 memt1 method_jit_args in
    let t2' = method_jit p t2 regt2 memt2 method_jit_args in
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
  | _ ->
    begin
      match Optimizer.optimize_exp p e reg mem with
      | Specialized (v) -> Ans (e)
      | Not_specialized (e, v) -> Ans (e)
    end

let exec p t reg mem jit_args =
  let t' = Simm.t t in
  begin match t' with
    | Let (_, IfEq (_, _,
                    Ans (CallDir (Id.L ("min_caml_jit_dispatch"), _, _)),
                    Ans (Nop)),
           interp_body) ->
      let Prog (table, fundefs, main) = p in
      let fundefs' = List.map fundefs (fun fundef ->
          let Id.L (x) = fundef.name in
          match String.split ~on:'.' x |> List.hd with
          | Some name' when name' = "interp" ->
            let { name; args; fargs; ret } = fundef in
            { name = name; args = args; fargs = fargs; body = interp_body; ret = ret }
          | _ -> fundef)
      in
      method_jit (Prog (table, fundefs', main)) interp_body reg mem jit_args
    | Ans _ | Let _ ->
      print_endline "come";
      method_jit p t reg mem jit_args
  end
  |> fun res ->
  { name = Id.L ("min_caml_test_trace")
  ; args = jit_args.reds
  ; fargs = []
  ; body = res
  ; ret = Type.Int }
