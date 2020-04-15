open Std
open MinCaml
open Asm
open Opt_lib

let rec is_occur var = function
  | Let (_, e, t) -> is_occur_exp var e || is_occur var t
  | Ans e ->
    (match e with
    | IfEq (x, V y, t1, t2) | IfLE (x, V y, t1, t2) | IfGE (x, V y, t1, t2) ->
      var = x || var = y || is_occur var t1 || is_occur var t2
    | IfEq (x, C _, t1, t2) | IfLE (x, C _, t1, t2) | IfGE (x, C _, t1, t2) ->
      var = x || is_occur var t1 || is_occur var t2
    | _ -> is_occur_exp var e)

and is_occur_exp (var : Id.t) (e : Asm.exp) : bool =
  match e with
  | Nop -> false
  | Mov x -> x = var
  | Add (x, V y) | Sub (x, V y) | Mul (x, V y) | Div (x, V y) | Mod (x, V y) ->
    x = var || y = var
  | Add (x, C _) | Sub (x, C _) | Mul (x, C _) | Div (x, C _) | Mod (x, C _) -> x = var
  | Ld (x, V y, _) -> x = var || y = var
  | Ld (x, C _, _) -> x = var
  | St (x, y, V z, _) -> x = var || y = var || z = var
  | St (x, y, C _, _) -> x = var || y = var
  | IfEq (x, V y, _, _) | IfLE (x, V y, _, _) | IfGE (x, V y, _, _) -> x = var || y = var
  | IfEq (x, C _, _, _) | IfLE (x, C _, _, _) | IfGE (x, C _, _, _) -> x = var
  | CallCls (x, args, fargs) -> x = var || List.mem var args || List.mem var fargs
  | CallDir (Id.L x, args, fargs) -> x = var || List.mem var args || List.mem var fargs
  | _ -> false
;;

let is_mode_checking = function
  | IfEq (x, _, _, _) | IfGE (x, _, _, _) | IfLE (x, _, _, _) ->
    let re = Str.regexp "mode\\.[a-zA-Z0-9]*" in
    Str.string_match re x 0
  | _ -> false
;;

let%test_module "is_occur test" =
  (module struct
    let t =
      Let
        ( ("Ti242.609", Int)
        , Sub ("sp.400", C 2)
        , Let
            ( ("Ti244.611", Int)
            , Sub ("sp.400", C 3)
            , Let
                ( ("v.612", Int)
                , Ld ("stack.399", V "Ti244.611", 4)
                , Let
                    ( ("Tu24.613", Unit)
                    , St ("v.612", "stack.399", V "sp.400", 4)
                    , Let
                        ( ("Ti246.615", Int)
                        , Add ("sp.400", C 1)
                        , Let
                            ( ("sp.400.848", Int)
                            , Add ("sp.400", C 1)
                            , Let
                                (("Ti333.507.868", Int), Set 0, Ans (Mov "Ti333.507.868"))
                            ) ) ) ) ) )
    ;;

    let%test _ = is_occur "Ti242.609" t = false
    let%test _ = is_occur "Ti333.507.868" t = true
  end)
;;

let specialize lhs rhs =
  Option.(
    match lhs, rhs with
    | Add (x, C n), Add (y, C m) -> Add (x, C (n + m)) |> some
    | Add (x, C n), Sub (y, C m) ->
      let v = n - m in
      (if v >= 0 then Add (x, C v) else Sub (x, C (-v))) |> some
    | Sub (x, C n), Add (y, C m) ->
      let v = n - m in
      (if v >= 0 then Add (x, C v) else Sub (x, C (-v))) |> some
    | Sub (x, C n), Sub (y, C m) -> Sub (x, C (n + m)) |> some
    | _ -> none)
;;

let rec find_greedy key env =
  let open Option in
  let rec find_greedy' key env =
    match M.find_opt key env with
    | Some v' -> find_greedy' v' env
    | None -> some key
  in
  match M.find_opt key env with
  | Some v' -> find_greedy' v' env
  | None -> none
[@@ocamlformat "disable"]

let rec const_fold_mov ?(env = M.empty) = function
  | Let ((var, typ), Mov x, t) ->
    pp "Folding: %s = Mov (%s)\n" var x;
    let env = M.add var x env in
    const_fold_mov ~env t
  | Let ((var, typ), e, t) ->
    (match e with
    | Add (x, C y) ->
      (match M.find_opt x env with
      | Some var2 -> Let ((var, typ), Add (var2, C y), const_fold_mov ~env t)
      | None -> Let ((var, typ), e, const_fold_mov ~env t))
    | Add (x, V y) ->
      pp "Folding: Add (%s, %s)\n" x y;
      (match M.find_opt x env, M.find_opt y env with
      | Some var1, Some var2 -> Let ((var, typ), Add (var1, V var2), const_fold_mov ~env t)
      | Some var1, None -> Let ((var, typ), Add (var1, V y), const_fold_mov ~env t)
      | None, Some var2 -> Let ((var, typ), Add (x, V var2), const_fold_mov ~env t)
      | None, None -> Let ((var, typ), Add (x, V y), const_fold_mov ~env t))
    | Sub (x, C y) ->
      (match M.find_opt x env with
      | Some var2 -> Let ((var, typ), Sub (var2, C y), const_fold_mov ~env t)
      | None -> Let ((var, typ), e, const_fold_mov ~env t))
    | Sub (x, V y) ->
      (match M.find_opt x env, M.find_opt y env with
      | Some var1, Some var2 -> Let ((var, typ), Sub (var1, V var2), const_fold_mov ~env t)
      | Some var1, None -> Let ((var, typ), Sub (var1, V y), const_fold_mov ~env t)
      | None, Some var2 -> Let ((var, typ), Sub (x, V var2), const_fold_mov ~env t)
      | None, None -> Let ((var, typ), Sub (x, V y), const_fold_mov ~env t))
    | Ld (x, V y, z) ->
      let e =
        match find_greedy x env, find_greedy y env with
        | Some x', Some y' -> Ld (x', V y', z)
        | Some x', None -> Ld (x', V y, z)
        | None, Some y' -> Ld (x, V y', z)
        | None, None -> Ld (x, V y, z)
      in
      Let ((var, typ), e, const_fold_mov ~env t)
    | St (x, y, V z, w) ->
      let e =
        match find_greedy x env, find_greedy y env, find_greedy z env with
        | Some x', Some y', Some z' -> St (x', y', V z', w)
        | Some x', Some y', None -> St (x', y', V z, w)
        | Some x', None, Some z' -> St (x', y, V z', w)
        | None, Some y', Some z' -> St (x, y', V z', w)
        | Some x', None, None -> St (x', y, V z, w)
        | None, Some y', None -> St (x, y', V z, w)
        | None, None, Some z' -> St (x, y, V z', w)
        | None, None, None -> St (x, y, V z, w)
      in
      Let ((var, typ), e, const_fold_mov ~env t)
    | IfEq (x, C y, t1, t2) | IfLE (x, C y, t1, t2) | IfGE (x, C y, t1, t2) ->
      let x_opt = M.find_opt x env in
      Let
        ( (var, typ)
        , (match x_opt with
          | Some x' -> e <=> (x', C y, const_fold_mov ~env t1, const_fold_mov ~env t2)
          | None -> e <=> (x, C y, const_fold_mov ~env t1, const_fold_mov ~env t2))
        , const_fold_mov ~env t )
    | IfEq (x, V y, t1, t2) | IfLE (x, V y, t1, t2) | IfGE (x, V y, t1, t2) ->
      let x_opt, y_opt = M.find_opt x env, M.find_opt y env in
      Let
        ( (var, typ)
        , (match x_opt, y_opt with
          | Some x', Some y' ->
            e <=> (x', V y', const_fold_mov ~env t1, const_fold_mov ~env t2)
          | Some x', None ->
            e <=> (x', V y, const_fold_mov ~env t1, const_fold_mov ~env t2)
          | None, Some y' ->
            e <=> (x, V y', const_fold_mov ~env t1, const_fold_mov ~env t2)
          | _ -> e <=> (x, V y, const_fold_mov ~env t1, const_fold_mov ~env t2))
        , const_fold_mov ~env t )
    | CallCls (x, args, fargs) ->
      Let
        ( (var, typ)
        , CallCls
            ( (if M.mem x env then M.find x env else x)
            , args |> List.map (fun x -> if M.mem x env then M.find x env else x)
            , fargs |> List.map (fun x -> if M.mem x env then M.find x env else x) )
        , const_fold_mov ~env t )
    | CallDir (id_l, args, fargs) ->
      Let
        ( (var, typ)
        , CallDir
            ( id_l
            , args |> List.map (fun x -> if M.mem x env then M.find x env else x)
            , fargs |> List.map (fun x -> if M.mem x env then M.find x env else x) )
        , const_fold_mov ~env t )
    | _ -> Let ((var, typ), e, const_fold_mov ~env t))
  | Ans e ->
    Ans
      (match e with
       | Mov x ->
         (match find_greedy x env with
          | Some x' -> Mov x'
          | None -> e)
      | IfEq (x, V y, t1, t2) | IfLE (x, V y, t1, t2) | IfGE (x, V y, t1, t2) ->
        let x_opt, y_opt = M.find_opt x env, M.find_opt y env in
        (match x_opt, y_opt with
        | Some x', Some y' ->
          e <=> (x', V y', const_fold_mov ~env t1, const_fold_mov ~env t2)
        | Some x', None -> e <=> (x', V y, const_fold_mov ~env t1, const_fold_mov ~env t2)
        | None, Some y' -> e <=> (x, V y', const_fold_mov ~env t1, const_fold_mov ~env t2)
        | _ -> e <=> (x, V y, const_fold_mov ~env t1, const_fold_mov ~env t2))
      | IfEq (x, C n, t1, t2) | IfLE (x, C n, t1, t2) | IfGE (x, C n, t1, t2) ->
        let x_opt = M.find_opt x env in
        (match x_opt with
        | Some x' -> e <=> (x', C n, const_fold_mov ~env t1, const_fold_mov ~env t2)
        | None -> e <=> (x, C n, const_fold_mov ~env t1, const_fold_mov ~env t2))
      | CallDir (id_l, args, fargs) ->
        CallDir
          ( id_l
          , args |> List.map (fun x -> if M.mem x env then M.find x env else x)
          , fargs |> List.map (fun x -> if M.mem x env then M.find x env else x) )
      | _ -> e)
;;

let rec elim_dead_exp = function
  | Let ((var, Type.Unit), e, t) ->
    (* side effect *)
    Let ((var, Type.Unit), e, elim_dead_exp t)
  | Let ((var, typ), e, t) ->
    if is_occur var t then Let ((var, typ), e, elim_dead_exp t) else elim_dead_exp t
  | Ans (IfEq (x, y, t1, t2)) -> Ans (IfEq (x, y, elim_dead_exp t1, elim_dead_exp t2))
  | Ans (IfLE (x, y, t1, t2)) -> Ans (IfLE (x, y, elim_dead_exp t1, elim_dead_exp t2))
  | Ans (IfGE (x, y, t1, t2)) -> Ans (IfGE (x, y, elim_dead_exp t1, elim_dead_exp t2))
  | Ans e -> Ans e
;;

let rec const_fold_id ?(env = M.empty) = function
  | Let ((var, typ), e, t) ->
    (match const_fold_id' env e with
    | `Folded x ->
      let env = M.add var x env in
      const_fold_id ~env t
    | `Not_folded e -> Let ((var, typ), e, const_fold_id ~env t))
  | Ans (IfEq (x, y, t1, t2) as e)
  | Ans (IfLE (x, y, t1, t2) as e)
  | Ans (IfGE (x, y, t1, t2) as e) ->
    Ans (e <=> (x, y, const_fold_id ~env t1, const_fold_id ~env t2))
  | Ans e ->
    (match const_fold_id' env e with `Folded x -> Ans (Mov x) | `Not_folded e -> Ans e)

and const_fold_id' env = function
  | Mov x ->
    (match M.find_opt x env with
     | Some x' -> `Folded x
     | None -> `Not_folded (Mov x))
  | Add (x, V y) ->
    `Not_folded
      (match M.find_opt x env, M.find_opt y env with
      | Some x', Some y' -> Add (x', V y')
      | Some x', None -> Add (x', V y)
      | None, Some y' -> Add (x, V y')
      | None, None -> Add (x, V y))
  | Add (x, C y) when y = 0 -> `Folded x
  | Add (x, C y) ->
    `Not_folded
      (match M.find_opt x env with Some x' -> Add (x', C y) | None -> Add (x, C y))
  | Sub (x, V y) ->
    `Not_folded
      (match M.find_opt x env, M.find_opt y env with
      | Some x', Some y' -> Sub (x', V y')
      | Some x', None -> Sub (x', V y)
      | None, Some y' -> Sub (x, V y')
      | None, None -> Sub (x, V y))
  | Sub (x, C y) when y = 0 -> `Folded x
  | Sub (x, C y) ->
    `Not_folded
      (match M.find_opt x env with Some x' -> Sub (x', C y) | None -> Sub (x, C y))
  | Ld (x, V y, z) ->
    `Not_folded
      (match M.find_opt x env, M.find_opt y env with
      | Some x', Some y' -> Ld (x', V y', z)
      | Some x', None -> Ld (x', V y, z)
      | None, Some y' -> Ld (x, V y', z)
      | None, None -> Ld (x, V y, z))
  | Ld (x, C y, z) ->
    `Not_folded
      (match M.find_opt x env with Some x' -> Ld (x', C y, z) | None -> Ld (x, C y, z))
  | St (x, y, V z, w) ->
    `Not_folded
      (match M.find_opt x env, M.find_opt y env, M.find_opt z env with
      | Some x', Some y', Some z' -> St (x', y', V z', w)
      | Some x', Some y', None -> St (x', y', V z, w)
      | Some x', None, Some z' -> St (x', y, V z', w)
      | None, Some y', Some z' -> St (x, y', V z', w)
      | Some x', None, None -> St (x', y, V z, w)
      | None, Some y', None -> St (x, y', V z, w)
      | None, None, Some z' -> St (x, y, V z', w)
      | None, None, None -> St (x, y, V z, w))
  | CallDir (id_l, args, fargs) ->
    let f = List.map (fun arg -> if M.mem arg env then M.find arg env else arg) in
    `Not_folded (CallDir (id_l, f args, f fargs))
  | CallCls (x, args, fargs) ->
    let f = List.map (fun arg -> if M.mem arg env then M.find arg env else arg) in
    let x = if M.mem x env then M.find x env else x in
    `Not_folded (CallCls (x, f args, f fargs))
  | e -> `Not_folded e
;;

let rec const_fold_exp ?(env = M.empty) =
  let extend_env x e env = M.add x e env in
  let mem x env = M.mem x env in
  function
  | Let ((var, typ), Mov x, t) when mem x env ->
    let exp' = M.find x env in
    let env = extend_env var exp' env in
    Let ((var, typ), exp', const_fold_exp ~env t)
  | Let ((var, typ), Add (x, y), t) when mem x env ->
    let lhs = M.find x env in
    (match specialize lhs (Add (x, y)) with
    | Some e_res ->
      let env = extend_env var e_res env in
      Let ((var, typ), e_res, const_fold_exp ~env t)
    | None ->
      let env = extend_env var (Add (x, y)) env in
      Let ((var, typ), Add (x, y), const_fold_exp ~env t))
  | Let ((var, typ), Sub (x, y), t) when mem x env ->
    let lhs = M.find x env in
    (match specialize lhs (Sub (x, y)) with
    | Some e_res ->
      let env = extend_env var e_res env in
      Let ((var, typ), e_res, const_fold_exp ~env t)
    | None ->
      let env = extend_env var (Sub (x, y)) env in
      Let ((var, typ), Sub (x, y), const_fold_exp ~env t))
  | Let ((var, typ), e, t) ->
    let env = extend_env var e env in
    Let ((var, typ), e, const_fold_exp ~env t)
  | Ans (IfEq (x, y, t1, t2) as e)
  | Ans (IfLE (x, y, t1, t2) as e)
  | Ans (IfGE (x, y, t1, t2) as e) ->
    if is_mode_checking e
    then Ans (e <=> (x, y, const_fold_exp ~env t1, const_fold_exp ~env t2))
    else if Opt_guard.is_guard_path t2
    then Ans (e <=> (x, y, const_fold_exp ~env t1, t2))
    else Ans (e <=> (x, y, t1, const_fold_exp ~env t2))
  | Ans (Mov x) when mem x env ->
    let exp' = M.find x env in
    Ans (exp')
  | Ans e -> Ans e
;;

let rec const_fold_if ?(env = M.empty) = function
  | Let (x, e, t) -> Let (x, e, const_fold_if ~env t)
  | Ans e ->
    (match e with
    | IfLE (x, y, t1, t2) | IfGE (x, y, t1, t2) | IfEq (x, y, t1, t2) ->
      if Opt_guard.is_guard_path t2
      then (
        let t = const_fold_exp ~env t1 |> const_fold_mov |> elim_dead_exp in
        Ans (e <=> (x, y, t, t2)))
      else (
        let t = const_fold_exp ~env t2 |> const_fold_mov |> elim_dead_exp in
        Ans (e <=> (x, y, t1, t)))
    | _ -> Ans e)
;;
