open Std
open MinCaml
open Asm
open Jit_env
open Jit_util

let ignored x ys = ys |> List.exists (fun y -> String.get_name x = y)

let rec ignore_hits = function
  | Ans exp -> Ans (ignore_hits_exp exp)
  | Let (x, CallDir (Id.L "min_caml_can_enter_jit", args, fargs), body) ->
    ignore_hits body
  | Let (x, CallDir (Id.L "min_caml_jit_merge_point", args, fargs), body) ->
    ignore_hits body
  | Let (x, exp, body) -> Let (x, ignore_hits_exp exp, ignore_hits body)

and ignore_hits_exp = function
  | IfEq (x, y, t1, t2) | SIfEq (x, y, t1, t2) ->
    IfEq (x, y, ignore_hits t1, ignore_hits t2)
  | IfGE (x, y, t1, t2) | SIfLE (x, y, t1, t2) ->
    IfGE (x, y, ignore_hits t1, ignore_hits t2)
  | IfLE (x, y, t1, t2) | SIfGE (x, y, t1, t2) ->
    IfLE (x, y, ignore_hits t1, ignore_hits t2)
  | exp -> exp
;;

let rec restore reg ~args cont =
  match args with
  | [] -> cont |> ignore_hits
  | hd :: tl ->
    (match reg.(int_of_id_t hd) with
    | Green n
      when String.get_name hd = "bytecode" || String.get_name hd = "code" ->
      Let
        ( (hd, Type.Int)
        , CallDir (Id.L "restore_min_caml_bp", [], [])
        , restore reg tl cont )
    | Green n -> Let ((hd, Type.Int), Set n, restore reg tl cont)
    | _ -> restore reg tl cont)
;;

let rec promote_interp tname = function
  | Ans e ->
    (match e with
    | CallDir (Id.L x, args, fargs) when String.contains x "interp" ->
      Ans (CallDir (Id.L ("guard_" ^ tname), args, fargs))
    | IfEq (x, y, t1, t2) | SIfEq (x, y, t1, t2) ->
      Ans (IfEq (x, y, promote_interp tname t1, promote_interp tname t2))
    | IfGE (x, y, t1, t2) | SIfGE (x, y, t1, t2) ->
      Ans (IfGE (x, y, promote_interp tname t1, promote_interp tname t2))
    | IfLE (x, y, t1, t2) | SIfLE (x, y, t1, t2) ->
      Ans (IfLE (x, y, promote_interp tname t1, promote_interp tname t2))
    | e -> Ans e)
  | Let (x, e, t) -> Let (x, e, promote_interp tname t)
;;

let rec promote reg ~trace_name:tname = function
  | Let (x, CallDir (Id.L "min_caml_guard_promote", args, fargs), body) ->
    restore reg ~args (Ans (CallDir (Id.L ("guard_" ^ tname), args, [])))
  | Let (x, e, body) -> promote reg tname body
  | Ans (CallDir (Id.L "min_caml_guard_promote", args, fargs)) ->
    restore reg ~args (Ans (CallDir (Id.L ("guard_" ^ tname), args, [])))
  | Ans _ -> failwith "Jit_guard.promote could not find min_caml_guard_promote."
;;

module TJ : sig
  val lookup : guard_pc:int -> [ `Pc of int ]
  val lookup_opt : guard_pc:int -> [ `Pc of int ] option
  val create : reg -> Jit_env.env -> ?wlist:'a list -> t -> t
end = struct
  (* merge_pc -> [guard_pc_1, gaurd_pc_2, ,,,] *)
  let guard_tbl : (int, int) Hashtbl.t = Hashtbl.create 100
  let register ~guard_pc ~merge_pc = Hashtbl.add guard_tbl guard_pc merge_pc
  let lookup ~guard_pc = `Pc (Hashtbl.find guard_tbl guard_pc)

  let lookup_opt ~guard_pc =
    Option.(bind (Hashtbl.find_opt guard_tbl guard_pc) (fun v -> some @@ `Pc v))
  ;;

  let is_pc x =
    let re_pc = Str.regexp "^pc\\.[a-zA-Z0-9\\.]*" in
    let re_addr = Str.regexp "^addr\\.[a-zA-Z0-9\\.]*" in
    Str.string_match re_pc x 0 || Str.string_match re_addr x 0
  ;;

  let rec insert_guard_occur_at (merge_pc, env) = function
    | Let ((var, typ), (Set n as e), t) when is_pc var ->
      let env = M.add var n env in
      Let ((var, typ), e, insert_guard_occur_at (merge_pc, env) t)
    | Let ((var, typ), (Add (x, C y) as e), t) when is_pc x ->
      let pc_v = M.find x env in
      let env = M.add var (pc_v + y) env in
      Let ((var, typ), e, insert_guard_occur_at (merge_pc, env) t)
    | Let ((var, typ), (Sub (x, C y) as e), t) when is_pc x ->
      let pc_v = M.find x env in
      let env = M.add var (pc_v - y) env in
      Let ((var, typ), e, insert_guard_occur_at (merge_pc, env) t)
    | Let ((var, typ), e, t) ->
      Let ((var, typ), e, insert_guard_occur_at (merge_pc, env) t)
    | Ans (CallDir (Id.L x, args, fargs) as e) ->
      Option.(
        bind
          (List.find_opt (fun arg -> M.mem arg env) args)
          (fun pc_arg ->
            bind (M.find_opt pc_arg env) (fun pc_v ->
                (* append the value of pc at this guard failer *)
                register ~guard_pc:pc_v ~merge_pc;
                Let
                  ( (Id.gentmp Type.Unit, Type.Unit)
                  , GuardAt pc_v
                  , Let
                      ( (Id.gentmp Type.Unit, Type.Unit)
                      , CallDir (Id.L "min_caml_guard_occur_at", args, [])
                      , Ans e ) )
                |> some))
        |> value ~default:(Ans e))
    | Ans e -> Ans e
  ;;

  let create reg env ?wlist:(ws = []) cont =
    let { merge_pc } = env in
    let free_vars = List.unique (fv cont) in
    restore reg free_vars cont
    (* too slow *)
    |> insert_guard_occur_at (merge_pc, M.empty)
    |> promote_interp env.trace_name
  ;;
end

module MJ : sig
  val create : reg -> env -> ?wlist:'a list -> t -> t
end = struct
  let rec create reg tj_env ?wlist:(ws = []) cont =
    let free_vars = List.unique (fv cont) in
    restore reg free_vars cont
  ;;
end
