open Std
open MinCaml
open Asm

open Printf

let ep = eprintf
let sp = sprintf
let pp = printf

let contains2 var (id_t, id_or_imm) =
  let open Asm in
  var = id_t || match id_or_imm with C n -> false | V x -> var = x
;;

let contains3 var (id_t1, id_t2, id_or_imm) =
  let open Asm in
  var = id_t1 || var = id_t2 || match id_or_imm with C n -> false | V x -> var = x
;;

let exists e_lhs e =
  match e_lhs, e with
  | Add (x_lhs, y_lhs), Add (x, y)
  | Add (x_lhs, y_lhs), Sub (x, y)
  | Sub (x_lhs, y_lhs), Add (x, y)
  | Sub (x_lhs, y_lhs), Sub (x, y) ->
    x_lhs = x || y_lhs = y
  | _ -> false
;;

(* rhs appears after lhs *)
let rec specialize_arith lhs rhs =
  match lhs, rhs with
  | Add (x, C n), Add (y, C m) ->
    (* w = x + n; z = w + m => z = x + (n + m) == Add (x, C (n + m)) *)
    Some (Add (x, C (n + m)))
  | Sub (x, C n), Sub (y, C m) ->
    (* w = x - n; z = w - m => z = x + (n + m) == Sub (x, C (n + m)) *)
    Some (Sub (x, C (n + m)))
  | Add (x, C n), Sub (y, C m) ->
    (* w = x + n; z = w - m => z = x + (n - m) == Add (x, C (n - m)) *)
    Some (Add (x, C (n - m)))
  | Sub (x, C n), Add (y, C m) ->
    (* w = x - n; z = w + m => z = x - (n - m) == Sub (x, C (n - m)) *)
    Some (Sub (x, C (n - m)))
  | e, Mov x -> Some e
  | _ -> None
;;

(* for tracing *)
let rec is_guard_path = function
  | Let (_, e, t) -> is_guard_path_exp e || is_guard_path t
  | Ans (e) -> is_guard_path_exp e

and is_guard_path_exp = function
  | CallDir (Id.L x, _, _) -> String.starts_with x "guard_"
  | _ -> false
;;

let%test "is_guard_path test" =
  let t =
    Let (("pc.402.1292", Int),  Set (16),
    Let (("bytecode.401.1293", Int),  CallDir (L "restore_min_caml_bp",[],[]),
    Let (("Ti195.686.1424", Int),  Add ("pc.402.1292",C 2 ),
    Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp2.683.1423"; "bytecode.401.1293"; "Ti195.686.1424"; ],[])))))
  in
  is_guard_path t = true
;;

(* remove e_lhs *)
(* specialize e_rhs *)
let rec remove_and_specialize e_lhs e_rhs = function
  | Let ((var, typ), e, t) when e = e_lhs -> remove_and_specialize e_lhs e_rhs t
  | Let ((var, typ), e, t) when e = e_rhs ->
    let e_opt = specialize_arith e_lhs e_rhs in
    (match e_opt with
    | Some v ->
      Let ((var, typ), v, remove_and_specialize e_lhs e_rhs t)
    | None ->
      Let ((var, typ), e, remove_and_specialize e_lhs e_rhs t))
  | Let ((var, typ), e, t) -> Let ((var, typ), e, remove_and_specialize e_lhs e_rhs t)
  | Ans (IfEq (x, y, t1, t2)) ->
    let t1 = remove_and_specialize e_lhs e_rhs t1 in
    let t2 = remove_and_specialize e_lhs e_rhs t2 in
    Ans (IfEq (x, y, t1, t2))
  | Ans (IfLE (x, y, t1, t2)) ->
    let t1 = remove_and_specialize e_lhs e_rhs t1 in
    let t2 = remove_and_specialize e_lhs e_rhs t2 in
    Ans (IfLE (x, y, t1, t2))
  | Ans (IfGE (x, y, t1, t2)) ->
    let t1 = remove_and_specialize e_lhs e_rhs t1 in
    let t2 = remove_and_specialize e_lhs e_rhs t2 in
    Ans (IfGE (x, y, t1, t2))
  | Ans e -> Ans e
;;

(* store in-progress states in "acc" *)
(* if it can be specialized, remove the "exp" from "acc" *)
(* TODO: how to handle if expression *)
let constfold t =
  let extend_env var exp env = M.add var exp env in
  let rec constfold_arith env acc = function
    | Let ((var, typ), e, t) ->
      (match e with
       | Mov x ->
         (match M.find_opt x env with
          | Some e_lhs ->
            (* ep "e_lhs: %s, x: %s\n" (show_exp e_lhs) x; *)
            let acc = remove_and_specialize e_lhs e acc in
            let env = extend_env var e_lhs env in
            constfold_arith env acc t
          | None ->
            let env = extend_env var e env in
            constfold_arith env acc t)
       | Add (x, y) | Sub (x, y) ->
         (* ep "finding x: %s\n" x; *)
         (match M.find_opt x env with
          | Some e_lhs ->
            (* ep "e_lhs: %s\n" (show_exp e_lhs); *)
            let acc = remove_and_specialize e_lhs e acc in
            let env = extend_env var e_lhs env in
            constfold_arith env acc t
          | None ->
            let env = extend_env var e env in
            constfold_arith env acc t)
       | _ ->
         let env = extend_env var e env in
         constfold_arith env acc t)
    | Ans e -> acc, env
  in
  let rec constfold_if env = function
    | Let (x, e, t) -> Let (x, e, constfold_if env t)
    | Ans (IfLE (x, y, t1, t2)) ->
      if is_guard_path t2 then
        let t, _ = constfold_arith env t1 t1 in
        Ans (IfLE (x, y, t, t2))
      else
        let t, _ = constfold_arith env t2 t2 in
        Ans (IfLE (x, y, t1, t))
    | Ans (IfGE (x, y, t1, t2)) ->
      if is_guard_path t2 then
        let t, _ = constfold_arith env t1 t1 in
        Ans (IfGE (x, y, t, t2))
      else
        let t, _ = constfold_arith env t2 t2 in
        Ans (IfGE (x, y, t1, t))
    | Ans (IfEq (x, y, t1, t2)) ->
      if is_guard_path t2 then
        let t, _ = constfold_arith env t1 t1 in
        Ans (IfEq (x, y, t, t2))
      else
        let t, _ = constfold_arith env t2 t2 in
        Ans (IfEq (x, y, t1, t))
    | Ans e -> Ans e
  in
  let res, env = constfold_arith M.empty t t in
  constfold_if env res
;;

let rec constfold_iter n asm_t =
  if n = 0 then asm_t else
    let t' = constfold asm_t in
    if t' = asm_t then asm_t
    else constfold_iter (n-1) t'
;;

let%test_module "constfold test" = (module struct
  let%test "constfold_arith test1" =
    let t_trace1 =
      Let (("Ti242.609", Int),  Sub ("sp.400",C 2 ),
      Let (("Ti244.611", Int),  Sub ("Ti242.609",C 1 ),
      Let (("v.612", Int),  Ld ("stack.399",V "Ti244.611",4),
      Let (("Tu24.613", Unit),  St ("v.612","stack.399",V "sp.400",4),
      Let (("Ti246.615", Int),  Add ("sp.400",C 1 ),
      Let (("sp.400.848", Int),  Mov ("Ti246.615"),
      Let (("Ti333.507.868", Int),  Set (0),
      Let (("Tu32.508.869", Unit),  St ("Ti333.507.868","stack.399",V "sp.400.848",4),
      Let (("Ti335.510.870", Int),  Add ("sp.400.848",C 1 ),
      Let (("sp.400.1071", Int),  Mov ("Ti335.510.870"),
      Let (("Ti149.724.1225", Int),  Sub ("sp.400.1071",C 1 ),
      Let (("v2.725.1226", Int),  Ld ("stack.399",V "Ti149.724.1225",4),
      Let (("Ti151.727.1227", Int),  Sub ("sp.400.1071",C 2 ),
      Let (("v1.728.1228", Int),  Ld ("stack.399",V "Ti151.727.1227",4),
      Let (("n.729.1229", Int),  IfLE ("v1.728.1228",V "v2.725.1226",
      Ans (Set (1)),
      Ans (Set (0))),
      Let (("Ti153.731.1230", Int),  Sub ("sp.400.1071",C 2 ),
      Let (("Tu12.732.1231", Unit),  St ("n.729.1229","stack.399",V "Ti153.731.1230",4),
      Let (("Ti155.734.1232", Int),  Sub ("sp.400.1071",C 1 ),
      Let (("sp.400.1294", Int),  Mov ("Ti155.734.1232"),
      Let (("Ti191.680.1421", Int),  Sub ("sp.400.1294",C 1 ),
      Let (("v.681.1422", Int),  Ld ("stack.399",V "Ti191.680.1421",4),
      Let (("sp2.683.1423", Int),  Sub ("sp.400.1294",C 1 ),
      Ans (IfEq ("v.681.1422",C 0 ,
      Let (("sp.400.1517", Int),  Mov ("sp2.683.1423"),
      Let (("Ti242.609.1606", Int),  Sub ("sp.400.1517",C 2 ),
      Let (("Ti244.611.1607", Int),  Sub ("Ti242.609.1606",C 1 ),
      Let (("v.612.1608", Int),  Ld ("stack.399",V "Ti244.611.1607",4),
      Let (("Tu24.613.1609", Unit),  St ("v.612.1608","stack.399",V "sp.400.1517",4),
      Let (("Ti246.615.1610", Int),  Add ("sp.400.1517",C 1 ),
      Let (("sp.400.1740", Int),  Mov ("Ti246.615.1610"),
      Let (("c.689.1872", Int),  Set (1),
      Let (("Tu15.690.1873", Unit),  St ("c.689.1872","stack.399",V "sp.400.1740",4),
      Let (("Ti184.692.1874", Int),  Add ("sp.400.1740",C 1 ),
      Let (("sp.400.1963", Int),  Mov ("Ti184.692.1874"),
      Let (("Ti85.799.2165", Int),  Sub ("sp.400.1963",C 1 ),
      Let (("v2.800.2166", Int),  Ld ("stack.399",V "Ti85.799.2165",4),
      Let (("Ti87.802.2167", Int),  Sub ("sp.400.1963",C 2 ),
      Let (("v1.803.2168", Int),  Ld ("stack.399",V "Ti87.802.2167",4),
      Let (("Ti89.805.2169", Int),  Sub ("sp.400.1963",C 2 ),
      Let (("Ti90.806.2170", Int),  Sub ("v1.803.2168",V "v2.800.2166"),
      Let (("Tu6.807.2171", Unit),  St ("Ti90.806.2170","stack.399",V "Ti89.805.2169",4),
      Let (("Ti92.809.2172", Int),  Sub ("sp.400.1963",C 1 ),
      Let (("sp.400.2186", Int),  Mov ("Ti92.809.2172"),
      Let (("Ti340.500.2201", Int),  Sub ("sp.400.2186",C 1 ),
      Let (("v.501.2202", Int),  Ld ("stack.399",V "Ti340.500.2201",4),
      Let (("Tu33.502.2203", Unit),  St ("v.501.2202","stack.399",V "sp.400.2186",4),
      Let (("Ti342.504.2204", Int),  Add ("sp.400.2186",C 1 ),
      Let (("sp.400.2409", Int),  Mov ("Ti342.504.2204"),
      Let (("Ti208.663.2524", Int),  Set (31),
      Let (("Tu21.664.2525", Unit),  St ("Ti208.663.2524","stack.399",V "sp.400.2409",4),
      Let (("Ti210.666.2526", Int),  Add ("sp.400.2409",C 1 ),
      Let (("Ti211.667.2527", Int),  Set (200),
      Let (("Tu20.668.2528", Unit),  St ("Ti211.667.2527","stack.399",V "Ti210.666.2526",4),
      Let (("sp2.670.2529", Int),  Add ("sp.400.2409",C 2 ),
      Ans (CallDir (L "tracetj0.844",["stack.399"; "sp2.670.2529"; ],[]))))))))))))))))))))))))))))))))),
      Let (("pc.402.1292", Int),  Set (16),
      Let (("bytecode.401.1293", Int),  CallDir (L "restore_min_caml_bp",[],[]),
      Let (("Ti195.686.1424", Int),  Add ("pc.402.1292",C 2 ),
      Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp2.683.1423"; "bytecode.401.1293"; "Ti195.686.1424"; ],[])))))))))))))))))))))))))))))
    in
    constfold_iter 1 t_trace1
    |> print_t;
    print_newline ();
    true
  ;;
end)

let rec find var t =
  match t with
  | Let (_, Mov x, t') when var = x -> true
  | Let (_, Add (x, y), t')
  | Let (_, Sub (x, y), t')
  | Let (_, Mul (x, y), t')
  | Let (_, Div (x, y), t')
  | Let (_, Mod (x, y), t')
    when contains2 var (x, y) ->
    true
  | Let (_, IfEq (x, y, t1, t2), t')
  | Let (_, IfLE (x, y, t1, t2), t')
  | Let (_, IfGE (x, y, t1, t2), t')
    when contains2 var (x, y) ->
    true
  | (Ans (IfEq (x, y, t1, t2)) | Ans (IfLE (x, y, t1, t2)) | Ans (IfGE (x, y, t1, t2)))
    when contains2 var (x, y) ->
    true
  | Let (_, e, t) -> find var t
  | Ans (IfEq (x, y, t1, t2)) | Ans (IfLE (x, y, t1, t2)) | Ans (IfGE (x, y, t1, t2)) ->
    find var t1 || find var t2
  | Ans _ -> false
;;

let%test "find test" =
  let t =
    Let
      ( ("x.2", Type.Int)
      , Add ("arg1.1", C 1)
      , Let (("y.3", Type.Int), Add ("x.2", C 3), Ans (Mov "y.3")) )
  in
  find "x.2" t = true
;;
