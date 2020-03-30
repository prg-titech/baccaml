open Std
open MinCaml
open Asm
open Printf
module M' = Map.Make (Int)

let ep = eprintf
let sp = sprintf
let pp = printf

let ( <=> ) e (x, y, t1, t2) =
  match e with
  | IfEq _ -> IfEq (x, y, t1, t2)
  | IfLE _ -> IfLE (x, y, t1, t2)
  | IfGE _ -> IfGE (x, y, t1, t2)
  | _ -> failwith "unexpected expression."
;;

let contains2 var (id_t, id_or_imm) =
  let open Asm in
  var = id_t || match id_or_imm with C n -> false | V x -> var = x
;;

let contains3 var (id_t1, id_t2, id_or_imm) =
  let open Asm in
  var = id_t1 || var = id_t2 || match id_or_imm with C n -> false | V x -> var = x
;;

let contains var e =
  match e with
  | Nop -> false
  | Set _ -> false
  | SetL (Id.L x) -> x = var
  | Mov x -> x = var
  | Add (x, y) | Sub (x, y) | Mul (x, y) | Div (x, y) | Mod (x, y) -> contains2 var (x, y)
  | Ld (x, y, z) -> contains2 var (x, y)
  | St (x, y, z, w) -> contains3 var (x, y, z)
  | IfEq (x, y, _, _) | IfGE (x, y, _, _) | IfLE (x, y, _, _) -> contains2 var (x, y)
  | CallCls (x, args, fargs) -> var = x || List.mem var args || List.mem var fargs
  | CallDir (Id.L x, args, fargs) -> var = x || List.mem var args || List.mem var fargs
  | _ -> false
;;

module Guard_opt : sig
  val is_guard_path : t -> bool
end = struct
  (* for tracing *)
  let rec is_guard_path = function
    | Let (_, e, t) -> is_guard_path_exp e || is_guard_path t
    | Ans e -> is_guard_path_exp e

  and is_guard_path_exp = function
    | CallDir (Id.L x, _, _) -> String.starts_with x "guard_"
    | _ -> false
  ;;

  let%test "is_guard_path test" =
    let t =
      Let
        ( ("pc.402.1292", Int)
        , Set 16
        , Let
            ( ("bytecode.401.1293", Int)
            , CallDir (L "restore_min_caml_bp", [], [])
            , Let
                ( ("Ti195.686.1424", Int)
                , Add ("pc.402.1292", C 2)
                , Ans
                    (CallDir
                       ( L "guard_tracetj0.844"
                       , [ "stack.399"
                         ; "sp2.683.1423"
                         ; "bytecode.401.1293"
                         ; "Ti195.686.1424"
                         ]
                       , [] )) ) ) )
    in
    is_guard_path t = true
  ;;

  let rec get_insts_inside_guard = function
    | Let ((var, typ), e, t) -> get_insts_inside_guard t
    | Ans (IfEq (x, y, t1, t2)) | Ans (IfLE (x, y, t1, t2)) | Ans (IfGE (x, y, t1, t2)) ->
      if is_guard_path t2
      then t2
      else if is_guard_path t1
      then t1
      else failwith "impossible application."
    | Ans e -> Ans e
  ;;

  let get_vars_inside_guard t =
    Asm.fv (get_insts_inside_guard t)
    |> List.filter (fun var ->
           let re = Str.regexp "stack.\\([a-zA-Z0-9]+\\)\\(\\.[a-ZA-Z0-9]*\\)*" in
           not (Str.string_match re var 0))
  ;;

  let rec get_insts_outside acc vars t =
    match t with
    | Let ((var, typ), e, t) ->
      if List.exists (fun var -> contains var e) vars || List.mem var vars
      then (
        let acc = Asm.concat (Ans e) (var, typ) acc in
        get_insts_outside acc vars t)
      else get_insts_outside acc vars t
    | Ans e -> acc
  ;;

  let move_into_guard t =
    let vars_inside_guard = get_vars_inside_guard t in
    let rec move_into_the_guard cand t =
      match t with
      | Let ((var, typ), e, t') ->
        if List.mem var vars_inside_guard
        then move_into_the_guard cand t'
        else Let ((var, typ), e, move_into_the_guard cand t')
      | Ans (IfEq (x, y, t1, t2) as e)
      | Ans (IfLE (x, y, t1, t2) as e)
      | Ans (IfGE (x, y, t1, t2) as e) ->
        if is_guard_path t1
        then (
          let t1' = Asm.concat cand (Id.gentmp Type.Unit, Type.Unit) t1 in
          Ans (e <=> (x, y, t1', t2)))
        else (
          let t2' = Asm.concat cand (Id.gentmp Type.Unit, Type.Unit) t2 in
          Ans (e <=> (x, y, t1, t2')))
      | Ans e -> Ans e
    in
    let insts_outside = get_insts_outside (Ans Nop) vars_inside_guard t in
    move_into_the_guard insts_outside t
  ;;

  let%test_module "move guard insts test" =
    (module struct
      let t1 =
        Let (("Ti244.611", Int),  Sub ("sp.400",C 3 ),
        Let (("v.612", Int),  Ld ("stack.399",V "Ti244.611",4),
        Let (("Tu24.613", Unit),  St ("v.612","stack.399",V "sp.400",4),
        Let (("sp.400.848", Int),  Add ("sp.400",C 1 ),
        Let (("Ti333.507.868", Int),  Set (0),
        Let (("Tu32.508.869", Unit),  St ("Ti333.507.868","stack.399",V "sp.400.848",4),
        Let (("Ti149.724.1225", Int),  Add ("sp.400",C 1 ),
        Let (("v2.725.1226", Int),  Mov ("Ti333.507.868"),
        Let (("Ti151.727.1227", Int),  Add ("sp.400",C 0 ),
        Let (("v1.728.1228", Int),  Mov ("v.612"),
        Let (("n.729.1229", Int),  IfLE ("v1.728.1228",V "v2.725.1226",
        Ans (Set (1)),
        Ans (Set (0))),
        Let (("Ti153.731.1230", Int),  Add ("sp.400",C 0 ),
        Let (("Tu12.732.1231", Unit),  St ("n.729.1229","stack.399",V "Ti153.731.1230",4),
        Let (("Ti191.680.1421", Int),  Add ("sp.400",C 0 ),
        Let (("v.681.1422", Int),  Mov ("n.729.1229"),
        Let (("sp2.683.1423", Int),  Add ("sp.400",C 0 ),
        Ans (IfEq ("v.681.1422",C 0 ,
        Let (("sp.400.1517", Int),  Add ("sp.400",C 0 ),
        Let (("Ti244.611.1607", Int),  Add ("sp.400",C (-3) ),
        Let (("v.612.1608", Int),  Ld ("stack.399",V "Ti244.611.1607",4),
        Let (("Tu24.613.1609", Unit),  St ("v.612.1608","stack.399",V "sp.400.1517",4),
        Let (("sp.400.1740", Int),  Add ("sp.400",C 1 ),
        Let (("c.689.1872", Int),  Set (1),
        Let (("Tu15.690.1873", Unit),  St ("c.689.1872","stack.399",V "sp.400.1740",4),
        Let (("Ti85.799.2165", Int),  Add ("sp.400",C 1 ),
        Let (("v2.800.2166", Int),  Mov ("c.689.1872"),
        Let (("Ti87.802.2167", Int),  Add ("sp.400",C 0 ),
        Let (("v1.803.2168", Int),  Mov ("v.612.1608"),
        Let (("Ti89.805.2169", Int),  Add ("sp.400",C 0 ),
        Let (("Ti90.806.2170", Int),  Sub ("v1.803.2168",V "v2.800.2166"),
        Let (("Tu6.807.2171", Unit),  St ("Ti90.806.2170","stack.399",V "Ti89.805.2169",4),
        Let (("sp.400.2186", Int),  Add ("sp.400",C 1 ),
        Let (("Ti340.500.2201", Int),  Add ("sp.400",C 0 ),
        Let (("v.501.2202", Int),  Mov ("Ti90.806.2170"),
        Let (("Tu33.502.2203", Unit),  St ("v.501.2202","stack.399",V "sp.400.2186",4),
        Let (("sp.400.2409", Int),  Add ("sp.400",C 2 ),
        Let (("Ti208.663.2524", Int),  Set (31),
        Let (("Tu21.664.2525", Unit),  St ("Ti208.663.2524","stack.399",V "sp.400.2409",4),
        Let (("Ti210.666.2526", Int),  Add ("sp.400",C 3 ),
        Let (("Ti211.667.2527", Int),  Set (200),
        Let (("Tu20.668.2528", Unit),  St ("Ti211.667.2527","stack.399",V "Ti210.666.2526",4),
        Let (("sp2.670.2529", Int),  Add ("sp.400",C 4 ),
        Ans (CallDir (L "tracetj0.844",["stack.399"; "sp2.670.2529"; ],[]))))))))))))))))))))))))))),
        Let (("pc.402.1292", Int),  Set (16),
        Let (("bytecode.401.1293", Int),  CallDir (L "restore_min_caml_bp",[],[]),
        Let (("Ti195.686.1424", Int),  Add ("pc.402.1292",C 2 ),
        Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp2.683.1423"; "bytecode.401.1293"; "Ti195.686.1424"; ],[])))))))))))))))))))))))
      [@@ocamlformat "disable"]

      let t1_moved =
        Let (("Ti244.611", Int),  Sub ("sp.400",C 3 ),
        Let (("v.612", Int),  Ld ("stack.399",V "Ti244.611",4),
        Let (("Tu24.613", Unit),  St ("v.612","stack.399",V "sp.400",4),
        Let (("sp.400.848", Int),  Add ("sp.400",C 1 ),
        Let (("Ti333.507.868", Int),  Set (0),
        Let (("Tu32.508.869", Unit),  St ("Ti333.507.868","stack.399",V "sp.400.848",4),
        Let (("Ti149.724.1225", Int),  Add ("sp.400",C 1 ),
        Let (("v2.725.1226", Int),  Mov ("Ti333.507.868"),
        Let (("Ti151.727.1227", Int),  Add ("sp.400",C 0 ),
        Let (("v1.728.1228", Int),  Mov ("v.612"),
        Let (("n.729.1229", Int),  IfLE ("v1.728.1228",V "v2.725.1226",
        Ans (Set (1)),
        Ans (Set (0))),
        Let (("Ti153.731.1230", Int),  Add ("sp.400",C 0 ),
        Let (("Tu12.732.1231", Unit),  St ("n.729.1229","stack.399",V "Ti153.731.1230",4),
        Let (("Ti191.680.1421", Int),  Add ("sp.400",C 0 ),
        Let (("v.681.1422", Int),  Mov ("n.729.1229"),
        Ans (IfEq ("v.681.1422",C 0 ,
        Let (("sp.400.1517", Int),  Add ("sp.400",C 0 ),
        Let (("Ti244.611.1607", Int),  Add ("sp.400",C (-3) ),
        Let (("v.612.1608", Int),  Ld ("stack.399",V "Ti244.611.1607",4),
        Let (("Tu24.613.1609", Unit),  St ("v.612.1608","stack.399",V "sp.400.1517",4),
        Let (("sp.400.1740", Int),  Add ("sp.400",C 1 ),
        Let (("c.689.1872", Int),  Set (1),
        Let (("Tu15.690.1873", Unit),  St ("c.689.1872","stack.399",V "sp.400.1740",4),
        Let (("Ti85.799.2165", Int),  Add ("sp.400",C 1 ),
        Let (("v2.800.2166", Int),  Mov ("c.689.1872"),
        Let (("Ti87.802.2167", Int),  Add ("sp.400",C 0 ),
        Let (("v1.803.2168", Int),  Mov ("v.612.1608"),
        Let (("Ti89.805.2169", Int),  Add ("sp.400",C 0 ),
        Let (("Ti90.806.2170", Int),  Sub ("v1.803.2168",V "v2.800.2166"),
        Let (("Tu6.807.2171", Unit),  St ("Ti90.806.2170","stack.399",V "Ti89.805.2169",4),
        Let (("sp.400.2186", Int),  Add ("sp.400",C 1 ),
        Let (("Ti340.500.2201", Int),  Add ("sp.400",C 0 ),
        Let (("v.501.2202", Int),  Mov ("Ti90.806.2170"),
        Let (("Tu33.502.2203", Unit),  St ("v.501.2202","stack.399",V "sp.400.2186",4),
        Let (("sp.400.2409", Int),  Add ("sp.400",C 2 ),
        Let (("Ti208.663.2524", Int),  Set (31),
        Let (("Tu21.664.2525", Unit),  St ("Ti208.663.2524","stack.399",V "sp.400.2409",4),
        Let (("Ti210.666.2526", Int),  Add ("sp.400",C 3 ),
        Let (("Ti211.667.2527", Int),  Set (200),
        Let (("Tu20.668.2528", Unit),  St ("Ti211.667.2527","stack.399",V "Ti210.666.2526",4),
        Let (("sp2.670.2529", Int),  Add ("sp.400",C 4 ),
        Ans (CallDir (L "tracetj0.844",["stack.399"; "sp2.670.2529"; ],[]))))))))))))))))))))))))))),
        Let (("sp2.683.1423", Int),  Add ("sp.400",C 0 ),
        Let (("Tu1", Unit), Nop,
        Let (("pc.402.1292", Int),  Set (16),
        Let (("bytecode.401.1293", Int),  CallDir (L "restore_min_caml_bp",[],[]),
        Let (("Ti195.686.1424", Int),  Add ("pc.402.1292",C 2 ),
        Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp2.683.1423"; "bytecode.401.1293"; "Ti195.686.1424"; ],[]))))))))))))))))))))))))
      [@@ocamlformat "disable"]

      let%test "get_insts_inside_guard test" =
        print_endline "";
        let r1 = get_insts_inside_guard t1 in
        let guard_branch =
          Let (("pc.402.1292", Int),  Set (16),
               Let (("bytecode.401.1293", Int),  CallDir (L "restore_min_caml_bp",[],[]),
                    Let (("Ti195.686.1424", Int),  Add ("pc.402.1292",C 2 ),
                         Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp2.683.1423"; "bytecode.401.1293"; "Ti195.686.1424"; ],[])))))
          [@ocamlformat "disable"]
        in
        r1 = guard_branch
      ;;

      let%test "get_vars_inside_guard test" =
        let r1 = get_vars_inside_guard t1 in
        r1 = [ "sp2.683.1423" ]
      ;;

      let%test "get_insts_outside test" =
        let vars = get_vars_inside_guard t1 in
        let r1 = get_insts_outside (Ans Nop) vars t1 in
        r1 = Let (("sp2.683.1423", Type.Int), Add ("sp.400", C 0), Ans Nop)
      ;;

      let%test "move_into_guard test" =
        let r1 = move_into_guard t1 in
        r1 = t1_moved
      ;;
    end)
  ;;
end

module Const_fold = struct
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
    | IfEq (x, V y, _, _) | IfLE (x, V y, _, _) | IfGE (x, V y, _, _) ->
      x = var || y = var
    | IfEq (x, C _, _, _) | IfLE (x, C _, _, _) | IfGE (x, C _, _, _) -> x = var
    | CallCls (x, args, fargs) -> x = var || List.mem var args || List.mem var fargs
    | CallDir (Id.L x, args, fargs) -> x = var || List.mem var args || List.mem var fargs
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
                                  ( ("Ti333.507.868", Int)
                                  , Set 0
                                  , Ans (Mov "Ti333.507.868") ) ) ) ) ) ) )
      ;;

      let%test _ = is_occur "Ti242.609" t = false
      let%test _ = is_occur "Ti333.507.868" t = true
    end)
  ;;

  let specialize lhs rhs =
    Option.(
      match lhs, rhs with
      | Add (x, C n), Add (y, C m) -> Add (x, C (n + m)) |> some
      | Add (x, C n), Sub (y, C m) -> Add (x, C (n - m)) |> some
      | Sub (x, C n), Add (y, C m) -> Sub (x, C (n - m)) |> some
      | Sub (x, C n), Sub (y, C m) -> Sub (x, C (n + m)) |> some
      | _ -> none)
  ;;

  let rec const_fold_mov env = function
    | Let ((var, typ), Mov x, t) ->
      (* pp "var: %s, x: %s\n" var x; *)
      let env = M.add var x env in
      const_fold_mov env t
    | Let ((var, typ), e, t) ->
      (match e with
      | Add (x, y) ->
        (match M.find_opt x env with
        | Some var2 -> Let ((var, typ), Add (var2, y), const_fold_mov env t)
        | None -> Let ((var, typ), e, const_fold_mov env t))
      | Sub (x, y) ->
        (match M.find_opt x env with
        | Some var2 -> Let ((var, typ), Sub (var2, y), const_fold_mov env t)
        | None -> Let ((var, typ), e, const_fold_mov env t))
      | Ld (x, V y, z) ->
        (match M.find_opt x env with
        | Some var2 -> Let ((var, typ), Ld (var2, V y, z), const_fold_mov env t)
        | None ->
          (match M.find_opt y env with
          | Some var2 -> Let ((var, typ), Ld (x, V var2, z), const_fold_mov env t)
          | None -> Let ((var, typ), e, const_fold_mov env t)))
      | St (x, y, V z, w) ->
        (match M.find_opt x env with
        | Some var2 -> Let ((var, typ), St (var2, y, V z, w), const_fold_mov env t)
        | None ->
          (match M.find_opt y env with
          | Some var2 -> Let ((var, typ), St (x, var2, V z, w), const_fold_mov env t)
          | None ->
            (match M.find_opt z env with
            | Some var2 -> Let ((var, typ), St (x, y, V var2, w), const_fold_mov env t)
            | None -> Let ((var, typ), e, const_fold_mov env t))))
      | CallCls (x, args, fargs) ->
        Let
          ( (var, typ)
          , CallCls
              ( (if M.mem x env then M.find x env else x)
              , args |> List.map (fun x -> if M.mem x env then M.find x env else x)
              , fargs |> List.map (fun x -> if M.mem x env then M.find x env else x) )
          , const_fold_mov env t )
      | CallDir (id_l, args, fargs) ->
        Let
          ( (var, typ)
          , CallDir
              ( id_l
              , args |> List.map (fun x -> if M.mem x env then M.find x env else x)
              , fargs |> List.map (fun x -> if M.mem x env then M.find x env else x) )
          , const_fold_mov env t )
      | _ -> Let ((var, typ), e, const_fold_mov env t))
    | Ans e ->
      Ans
        (match e with
        | IfEq (x, V y, t1, t2) | IfLE (x, V y, t1, t2) | IfGE (x, V y, t1, t2) ->
          let x_opt, y_opt = M.find_opt x env, M.find_opt y env in
          (match x_opt, y_opt with
          | Some x', None -> e <=> (x', V y, const_fold_mov env t1, const_fold_mov env t2)
          | None, Some y' -> e <=> (x, V y', const_fold_mov env t1, const_fold_mov env t2)
          | _ -> e <=> (x, V y, const_fold_mov env t1, const_fold_mov env t2))
        | IfEq (x, C n, t1, t2) | IfLE (x, C n, t1, t2) | IfGE (x, C n, t1, t2) ->
          let x_opt = M.find_opt x env in
          (match x_opt with
          | Some x' -> e <=> (x', C n, const_fold_mov env t1, const_fold_mov env t2)
          | None -> e <=> (x, C n, const_fold_mov env t1, const_fold_mov env t2))
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

  (* remove Add (x, C 0), Sub (x, C 0), etc. *)
  (* TODO: move expressions related to guard operation *)
  let rec const_fold_identity t =
    let rec const_fold_identity' t =
      match t with
      | Let ((var, typ), e, t) ->
        (match const_fold_identity_exp' e with
        | `Identity x -> Let ((var, typ), Mov x, const_fold_identity' t)
        | `Not_identity -> Let ((var, typ), e, const_fold_identity' t))
      | Ans e ->
        (match e with
        | IfEq (x, y, t1, t2) | IfLE (x, y, t1, t2) | IfGE (x, y, t1, t2) ->
          Ans (e <=> (x, y, const_fold_identity' t1, const_fold_identity' t2))
        | _ -> Ans e)
    and const_fold_identity_exp' e =
      match e with Add (x, C 0) | Sub (x, C 0) -> `Identity x | _ -> `Not_identity
    in
    const_fold_identity' t |> const_fold_mov M.empty
  ;;

  let rec const_fold env =
    let extend_env x e env = M.add x e env in
    let mem x env = M.mem x env in
    function
    | Let ((var, typ), Mov x, t) when mem x env ->
      let exp' = M.find x env in
      let env = extend_env var exp' env in
      Let ((var, typ), exp', const_fold env t)
    | Let ((var, typ), Add (x, y), t) when mem x env ->
      let lhs = M.find x env in
      (match specialize lhs (Add (x, y)) with
      | Some e_res ->
        let env = extend_env var e_res env in
        Let ((var, typ), e_res, const_fold env t)
      | None ->
        let env = extend_env var (Add (x, y)) env in
        Let ((var, typ), Add (x, y), const_fold env t))
    | Let ((var, typ), Sub (x, y), t) when mem x env ->
      let lhs = M.find x env in
      (match specialize lhs (Sub (x, y)) with
      | Some e_res ->
        let env = extend_env var e_res env in
        Let ((var, typ), e_res, const_fold env t)
      | None ->
        let env = extend_env var (Sub (x, y)) env in
        Let ((var, typ), Sub (x, y), const_fold env t))
    | Let ((var, typ), e, t) ->
      let env = extend_env var e env in
      Let ((var, typ), e, const_fold env t)
    | Ans e ->
      (match e with
      | IfEq (x, y, t1, t2) | IfGE (x, y, t1, t2) | IfLE (x, y, t1, t2) ->
        if Guard_opt.is_guard_path t2
        then Ans (e <=> (x, y, const_fold env t1, t2))
        else Ans (e <=> (x, y, t1, const_fold env t2))
      | _ -> Ans e)
  ;;

  let rec const_fold_if env = function
    | Let (x, e, t) -> Let (x, e, const_fold_if env t)
    | Ans e ->
      (match e with
      | IfLE (x, y, t1, t2) | IfGE (x, y, t1, t2) | IfEq (x, y, t1, t2) ->
        if Guard_opt.is_guard_path t2
        then (
          let t = const_fold env t1 |> const_fold_mov M.empty |> elim_dead_exp in
          Ans (e <=> (x, y, t, t2)))
        else (
          let t = const_fold env t2 |> const_fold_mov M.empty |> elim_dead_exp in
          Ans (e <=> (x, y, t1, t)))
      | _ -> Ans e)
  ;;
end

module Mem_opt : sig
  val remove_rw : int M.t -> string M'.t -> t -> t
  val find_remove_candidate : int M.t -> (string * exp) M'.t -> exp M.t -> t -> exp M.t
  val remove_unread_write : exp M.t -> t -> t
  val const_fold_rw : t -> t
end = struct
  let check_sp sp =
    let sp_strs = String.split_on_char '.' sp in
    List.hd sp_strs = "sp" && List.length sp_strs = 2
  ;;

  let check_stack id =
    let stk_strs = String.split_on_char '.' id in
    List.hd stk_strs = "stack" && List.length stk_strs = 2
  ;;

  let rec remove_rw (sp_env : int M.t) (mem_env : string M'.t) = function
    | Let ((var, typ), (Add (x, C n) as e), t) when check_sp x ->
      let sp_env = M.add var n sp_env in
      Let ((var, typ), e, remove_rw sp_env mem_env t)
    | Let ((var, typ), (Sub (x, C n) as e), t) when check_sp x ->
      let sp_env = M.add var (-n) sp_env in
      Let ((var, typ), e, remove_rw sp_env mem_env t)
    | Let ((var, typ), (St (x, y, V z, w) as e), t) when check_stack y && check_sp z ->
      let mem_env = M'.add 0 x mem_env in
      Let ((var, typ), e, t |> remove_rw sp_env mem_env)
    | Let ((var, typ), (St (x, y, V z, w) as e), t) when check_stack y ->
      (try
         let sp = M.find z sp_env in
         let mem_env = M'.add sp x mem_env in
         Let ((var, typ), e, t |> remove_rw sp_env mem_env)
       with
      | Not_found -> Let ((var, typ), e, t |> remove_rw sp_env mem_env))
    | Let ((var, typ), (Ld (x, V y, z) as e), t) when check_stack x && check_sp y ->
      (try
         let addr = M'.find 0 mem_env in
         Let ((var, typ), Mov addr, t |> remove_rw sp_env mem_env)
       with
      | Not_found -> Let ((var, typ), e, t |> remove_rw sp_env mem_env))
    | Let ((var, typ), (Ld (x, V y, z) as e), t) when check_stack x ->
      (try
         let sp = M.find y sp_env in
         let addr = M'.find sp mem_env in
         Let ((var, typ), Mov addr, t |> remove_rw sp_env mem_env)
       with
      | Not_found -> Let ((var, typ), e, t |> remove_rw sp_env mem_env))
    | Let ((var, typ), e, t) -> Let ((var, typ), e, t |> remove_rw sp_env mem_env)
    | Ans (IfEq (x, y, t1, t2) as e)
    | Ans (IfLE (x, y, t1, t2) as e)
    | Ans (IfGE (x, y, t1, t2) as e) ->
      Ans (e <=> (x, y, remove_rw sp_env mem_env t1, remove_rw sp_env mem_env t2))
    | Ans e -> Ans e
  ;;

  let rec find_remove_candidate
      (sp_env : int M.t)
      (mem_env : (string * exp) M'.t)
      (remove_cand : exp M.t)
    = function
    | Let ((var, typ), Add (x, C n), t) when check_sp x ->
      let sp_env = M.add var n sp_env in
      find_remove_candidate sp_env mem_env remove_cand t
    | Let ((var, typ), Sub (x, C n), t) when check_sp x ->
      let sp_env = M.add var (-n) sp_env in
      find_remove_candidate sp_env mem_env remove_cand t
    | Let ((var, typ), (St (x, y, V z, w) as e), t) when check_stack y && check_sp z ->
      (match M'.find_opt 0 mem_env with
      | Some (var', e') ->
        let remove_cand = M.add var' e' remove_cand in
        let mem_env = M'.add 0 (var, e) mem_env in
        t |> find_remove_candidate sp_env mem_env remove_cand
      | None ->
        let mem_env = M'.add 0 (var, e) mem_env in
        t |> find_remove_candidate sp_env mem_env remove_cand)
    | Let ((var, typ), (St (x, y, V z, w) as e), t) when check_stack y ->
      (try
         let sp = M.find z sp_env in
         match M'.find_opt sp mem_env with
         | Some (var', e') ->
           let remove_cand = M.add var' e' remove_cand in
           let mem_env = M'.add sp (var, e) mem_env in
           t |> find_remove_candidate sp_env mem_env remove_cand
         | None ->
           let mem_env = M'.add sp (var, e) mem_env in
           t |> find_remove_candidate sp_env mem_env remove_cand
       with
      | Not_found -> t |> find_remove_candidate sp_env mem_env remove_cand)
    | Let ((var, typ), e, t) -> t |> find_remove_candidate sp_env mem_env remove_cand
    | Ans (IfEq (x, y, t1, t2)) | Ans (IfLE (x, y, t1, t2)) | Ans (IfGE (x, y, t1, t2)) ->
      let f = find_remove_candidate sp_env mem_env remove_cand in
      if Guard_opt.is_guard_path t1 then f t2 else f t1
    | Ans e -> remove_cand
  ;;

  let rec remove_unread_write cand = function
    | Let ((var, typ), e, t) when M.mem var cand -> remove_unread_write cand t
    | Let ((var, typ), e, t) -> Let ((var, typ), e, remove_unread_write cand t)
    | Ans (IfEq (x, y, t1, t2) as e)
    | Ans (IfLE (x, y, t1, t2) as e)
    | Ans (IfGE (x, y, t1, t2) as e) ->
      Ans (e <=> (x, y, remove_unread_write cand t1, remove_unread_write cand t2))
    | Ans e -> Ans e
  ;;

  let const_fold_rw t =
    let t1 = remove_rw M.empty M'.empty t in
    let cand = find_remove_candidate M.empty M'.empty M.empty t1 in
    remove_unread_write cand t1
  ;;
end
