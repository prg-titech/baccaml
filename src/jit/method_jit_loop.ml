open Mincaml
open Util
open Asm
open Core
open Inlining
open Jit_config
open Jit_util
open Renaming


let rec find_loop_start_pc p reg mem = function
  | Ans (exp) -> find_loop_start_pc' p reg mem exp
  | Let ((dest, typ), CallDir (Id.L "min_caml_loop_start", args, fargs), body) ->
    begin match List.hd args with
      | Some (v) ->
        [reg.(int_of_id_t v) |> value_of] @ (find_loop_start_pc p reg mem body)
      | None ->
        show_exp (CallDir (Id.L "min_caml_loop_start", args, fargs)) |> Logger.debug;
        []
    end
  | Let ((dest, typ), CallDir (Id.L "min_caml_loop_end", args, fargs), body) ->
    []
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) ->
    find_loop_start_pc p reg mem body
  | Let ((dest, typ), exp, body) ->
    begin match Optimizer.optimize_exp p exp reg mem with
      | Specialized (v) | Not_specialized (_, v) ->
        reg.(int_of_id_t dest) <- v;
        find_loop_start_pc p reg mem body
    end
    

and find_loop_start_pc' p reg mem = function
  | CallDir (id_l, args, fargs) ->
    begin match id_l with
    | Id.L ("min_caml_loop_start") ->
      begin match List.hd args with
      | Some (v) -> [reg.(int_of_id_t v) |> value_of]
      | None ->
        show_exp (CallDir (Id.L "min_caml_loop_start", args, fargs)) |> Logger.debug;
        []
      end
    | Id.L ("min_caml_loop_end") ->
      []
    | _ ->
      Logger.debug (Printf.sprintf "loop_find: CallDir (%s)" (string_of_id_l id_l));
      let fundef = find_fundef p id_l in
      let t = Inlining.inline_calldir_exp args fundef reg in
      find_loop_start_pc p reg mem t
    end
  | IfEq (id_t, id_or_imm, t1, t2) | IfGE (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) ->
    let reg1, reg2 = Array.copy reg, Array.copy reg in
    let mem1, mem2 = Array.copy mem, Array.copy mem in
    (find_loop_start_pc p reg1 mem1 t1) @ (find_loop_start_pc p reg2 mem2 t2)
  | exp ->
    []
        
