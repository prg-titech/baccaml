
open Std
open Base
open Asm
open Inlining
open Renaming
open Jit_env
open Jit_util
open Operands

(* function_name -> (arguments, following expressions) *)
module M = Map.Make (String)

let merge_pc = ref 0

let red_names = ref [""]

let rec restore_and_concat args reg cont =
  match args with
  | [] -> cont
  | hd :: tl ->
     let jv = reg.(int_of_id_t hd) in
     if String.split_on_char '.' hd |> List.hd = "bytecode" then
       Let ( (hd, Type.Int)
           , CallDir (Id.L "restore_min_caml_bp", [], [])
           , restore_and_concat tl reg cont)
     else if is_green jv then
       Let ( (hd, Type.Int)
           , Set (value_of jv)
           , restore_and_concat tl reg cont)
     else restore_and_concat tl reg cont

let get_names =
  List.map
    (fun id ->
      try String.index id '.' |> Str.string_before id
      with e -> id)

let filter ~reds =
  List.filter
    (fun id ->
      let name = String.split_on_char '.' id |> List.hd in
      List.mem name reds)

let rec mj p reg mem env = function
  | Ans exp -> mj_exp p reg mem env exp
  | Let ((dest, typ), CallDir (Id.L "min_caml_tracing_fail", args, fargs), body) ->
     failwith "tracing failed."
  | Let ((dest, typ), CallDir (Id.L "min_caml_loop_start", args, fargs), body) ->
     failwith "loop_start is not supported."
  | Let ((dest, typ), CallDir (Id.L "min_caml_loop_end", args, fargs), body) ->
     failwith "loop_end is not supported."
  | Let ((dest, typ), CallDir (Id.L ("min_caml_method_entry"), args, fargs), body) ->
     Log.debug ("min_caml_method_entry");
     mj p reg mem env body
  | Let ((dest, typ), CallDir (Id.L ("min_caml_jit_merge_point"), args, fargs), body) ->
     let pc = List.hd args |> int_of_id_t |> Array.get reg |> value_of in
     Log.debug ("jit_merge_point pc: " ^ string_of_int pc);
     mj p reg mem env body
  | Let ((dest, typ), CallDir (Id.L ("min_caml_mj_call"), args, fargs), body) ->
     let pc = List.nth args env.index_pc |> int_of_id_t |> Array.get reg |> value_of in
     if pc = env.merge_pc then (
       (Let ( (dest, typ)
            , CallDir (Id.L env.trace_name
                     , filter ~reds:(get_names (env.red_names)) args, fargs)
            , mj p reg mem env body))
     ) else
       let interp = Fundef.find_fuzzy p "interp" |> fun { name } -> name in
       (Let ( (dest, typ)
            , CallDir (interp, args, fargs)
            , mj p reg mem env body)) |> restore_and_concat args reg
  | Let ((dest, typ), CallDir (Id.L ("min_caml_can_enter_jit"), args, fargs), body) ->
     mj p reg mem env body
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) ->
     let callee =
       Fundef.find p id_l
       |> Inlining.inline_fundef reg args
       |> mj p reg mem env
     in
     let succ = mj p reg mem env body in
     Asm.concat callee (dest, typ) succ
  | Let ((dest, typ), exp, body) ->
    match exp with
    | (IfEq _ | IfGE _ | IfLE _ | SIfEq _ | SIfLE _ | SIfGE _) ->
       let t' = mj_if p reg mem env exp in
       let k = mj p reg mem env body in
       Asm.concat t' (dest, typ) k
    | St (id_t1, id_t2, id_or_imm, x) ->
      let srcv = reg.(int_of_id_t id_t1) in
      let destv = reg.(int_of_id_t id_t2) in
      let offsetv =
        match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n
      in
      let body' = mj p reg mem env body in
      begin match (srcv, destv) with
      | Green n1, Red n2 | LightGreen n1, Red n2 ->
        reg.(int_of_id_t dest) <- Green 0 ;
        mem.(n1 + (n2 * x)) <- Green (int_of_id_t id_t1 |> Array.get reg |> value_of) ;
        begin match offsetv with
        | Green n | LightGreen n -> (
           let id' = Id.gentmp Type.Int in
           Let ( (id_t1, Type.Int)
               , Set n1
               , Let ( (id', Type.Int)
                     , Set n
                     , Let ((dest, typ), St (id_t1, id_t2, C n, x), body'))))
        | Red n ->
           Let ( (id_t1, Type.Int)
               , Set n1
               , Let ((dest, typ), St (id_t1, id_t2, id_or_imm, x), body') )
        end
      | _ -> optimize_exp p exp reg mem (dest, typ) env body
      end
    | _ -> optimize_exp p exp reg mem (dest, typ) env body

and optimize_exp p exp reg mem (dest, typ) env body =
  match Jit_optimizer.run p exp reg mem with
  | Specialized v ->
     reg.(int_of_id_t dest) <- v ;
     mj p reg mem env body
  | Not_specialized (e, v) ->
     reg.(int_of_id_t dest) <- v ;
     let t = mj p reg mem env body in
     Let ((dest, typ), e, t)

and mj_exp p reg mem env = function
  | CallDir (id_l, args, fargs) ->
     Log.debug (Printf.sprintf "CallDir (%s)" (Id.string_of_id_l id_l)) ;
     let fundef = Fundef.find p id_l in
     let t = Inlining.inline_fundef reg args fundef in
     mj p reg mem env t
  | (IfEq _ | IfGE _ | IfLE _ | SIfEq _ | SIfLE _ | SIfGE _) as exp ->
     mj_if p reg mem env exp
  | exp ->
    begin match Jit_optimizer.run p exp reg mem with
    | Specialized v ->
       let id = Id.gentmp Type.Int in
       Let ((id, Type.Int), Set (value_of v), Ans (Mov id))
    | Not_specialized (e, v) -> Ans e
    end

and mj_if p reg mem env = function
  | IfGE (id_t, id_or_imm, t1, t2)
  | IfEq (id_t, id_or_imm, t1, t2)
  | IfLE (id_t, id_or_imm, t1, t2) as exp when let name = String.get_name id_t in name = "mode" ->
     Log.debug (Printf.sprintf "If (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
     Log.debug (Printf.sprintf "mode: %d" (int_of_id_or_imm id_or_imm |> Array.get reg |> value_of));
     let mode = value_of reg.(int_of_id_t id_t) in (* mj: 100, tj: 200 *)
     let r2 = match id_or_imm with V id -> value_of reg.(int_of_id_t id) | C n -> n in
     (* [XXX] improve guard failure *)
     if mode = 100 then
       Ans (exp |%%| (Ans (Nop), mj p reg mem env t2))
     else
       Ans (exp |%%| (mj p reg mem env t1, Ans (Nop)))
  | IfGE (id_t, id_or_imm, t1, t2)
  | IfEq (id_t, id_or_imm, t1, t2)
  | IfLE (id_t, id_or_imm, t1, t2) as exp ->
     Log.debug
       (Printf.sprintf "If (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm)) ;
     let r1 = value_of reg.(int_of_id_t id_t) in
     let r2 = match id_or_imm with V id -> value_of reg.(int_of_id_t id) | C n -> n in
     if exp |*| (r1, r2)
     then mj p reg mem env t1
     else mj p reg mem env t2
  | SIfEq (id_t, id_or_imm, t1, t2)
  | SIfLE (id_t, id_or_imm, t1, t2)
  | SIfGE (id_t, id_or_imm, t1, t2) as exp ->
     Log.debug (Printf.sprintf "If (%s, %s, t1, t2)" id_t (string_of_id_or_imm id_or_imm));
     let r1 = reg.(int_of_id_t id_t) in
     let r2 = match id_or_imm with V id -> reg.(int_of_id_t id) | C n -> Green n in
     let regt1, regt2 = (Array.copy reg, Array.copy reg) in
     let memt1, memt2 = (Array.copy mem, Array.copy mem) in
     let t1' = mj p regt1 memt1 env t1 in
     let t2' = mj p regt2 memt2 env t2 in
     begin match (r1, r2) with
     | Green n1, Green n2
     | LightGreen n1, LightGreen n2
     | Green n1, LightGreen n2
     | LightGreen n1, Green n2 -> if exp |*| (n1, n2) then t1' else t2'
     | Red n1, Green n2 | Red n1, LightGreen n2 -> Ans (exp |%| (id_t, C n2, t1', t2'))
     | Green n1, Red n2 | LightGreen n1, Red n2 ->
        let id_t2 = match id_or_imm with
          | V id -> id
          | C n -> failwith "id_or_imm should be string"
        in
        Ans (exp |%| (id_t2, C n1, t2', t1'))
     | Red n1, Red n2 -> Ans (exp |%| (id_t, id_or_imm, t1', t2'))
     end
  | _ -> failwith "method_jit_if should accept conditional branches."


let run prog reg mem ({trace_name; red_names; index_pc= x; merge_pc= y} as env) =
  Id.counter := 0; Renaming.counter := 0;
  let { args; body } = Fundef.find_fuzzy prog "interp" in
  let trace = mj prog reg mem env body in
  { name= Id.L env.trace_name
  ; args= args |> List.filter (fun arg -> List.mem (String.get_name arg) red_names)
  ; fargs= []
  ; body= trace
  ; ret= Type.Int }
