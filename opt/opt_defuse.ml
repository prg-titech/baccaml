open MinCaml
open Asm

let contains2 var (id_t, id_or_imm) =
  let open Asm in
  var = id_t || match id_or_imm with C n -> false | V x -> var = x
;;

let contains3 var (id_t1, id_t2, id_or_imm) =
  let open Asm in
  var = id_t1 || var = id_t2 || match id_or_imm with C n -> false | V x -> var = x
;;

(* rhs appears after lhs *)
let rec specialize_arith lhs rhs =
  match lhs, rhs with
  | Add (x, C n), Add (y, C m) -> Some (Add (x, C (n + m)))
  | Sub (x, C n), Sub (y, C m) -> Some (Sub (x, C (n + m)))
  | Add (x, C n), Sub (y, C m) -> Some (Add (x, C (n - m)))
  | Sub (x, C n), Add (y, C m) -> Some (Sub (x, C (n - m)))
  | e, Mov x -> Some e
  | _ -> None
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

(* remove e_lhs *)
(* specialize e_rhs *)
let rec remove_and_specialize e_lhs e_rhs = function
  | Let ((var, typ), e, t) when e = e_lhs -> remove_and_specialize e_lhs e_rhs t
  | Let ((var, typ), e, t) when e = e_rhs ->
    Printf.eprintf "e_lhs, e: %s, %s\n" (show_exp e_lhs) (show_exp e);
    let e_opt = specialize_arith e_lhs e_rhs in
    (match e_opt with
    | Some v ->
      Printf.eprintf "Specialized: %s\n" (Asm.show_exp v);
      Let ((var, typ), v, remove_and_specialize e_lhs e_rhs t)
    | None -> Let ((var, typ), e, remove_and_specialize e_lhs e_rhs t))
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
let constfold_arith t =
  let rec constfold_arith' env acc = function
    | Let ((var, typ), e, t) ->
      (match e with
       | Mov x ->
         (match M.find_opt x env with
          | Some e_lhs ->
            let acc = remove_and_specialize e_lhs e acc in
            constfold_arith' env acc t
          | None ->
            let env = M.add var e env in
            constfold_arith' env acc t)
       | Add (x, y) | Sub (x, y) ->
         (match M.find_opt x env with
          | Some e_lhs ->
            let acc = remove_and_specialize e_lhs e acc in
            constfold_arith' env acc t
          | None ->
            let env = M.add var e env in
            constfold_arith' env acc t)
       | _ ->
         let env = M.add var e env in
         constfold_arith' env acc t)
    | Ans e -> acc
  in
  constfold_arith' M.empty t t
;;

let%test "constfold_arith test1" =
  let t_wo_if =
    Let
      ( ("Ti242.609", Int)
      , Sub ("sp.400", C 2)
      , Let
          ( ("Ti244.611", Int)
          , Sub ("Ti242.609", C 1)
          , Let
              ( ("v.612", Int)
              , Ld ("stack.399", V "Ti244.611", 4)
              , Let
                  ( ("Tu24.613", Unit)
                  , St ("v.612", "stack.399", V "sp.400", 4)
                  , Let (("Ti246.615", Int), Add ("sp.400", C 1),
                         Ans (Set (-100))) ) ) )
      )
  in
  let t_trace1 =
     Let (("Ti242.609", Int),  Sub ("sp.400",C 2 ),
     Let (("Ti244.611", Int),  Sub ("Ti242.609",C 1 ),
     Let (("v.612", Int),  Ld ("stack.399", V ("Ti244.611"),4),
     Let (("Tu24.613", Unit),  St ("v.612","stack.399",V ("sp.400"),4),
     Let (("Ti246.615", Int),  Add ("sp.400",C 1 ),
     Let (("sp.400.848", Int),  Mov ("Ti246.615"),
     Let (("Ti333.507.868", Int),  Set (0),
     Let (("Tu32.508.869", Unit),  St ("Ti333.507.868","stack.399",V ("sp.400.848"),4),
     Let (("Ti335.510.870", Int),  Add ("sp.400.848",C 1 ),
     Let (("sp.400.1071", Int),  Mov ("Ti335.510.870"),
     Let (("Ti149.724.1225", Int),  Sub ("sp.400.1071",C 1 ),
     Let (("v2.725.1226", Int),  Ld ("stack.399",V ("Ti149.724.1225"),4),
     Let (("Ti151.727.1227", Int),  Sub ("sp.400.1071",C 2 ),
     Let (("v1.728.1228", Int),  Ld ("stack.399",V ("Ti151.727.1227"), 4),
     Let (("n.729.1229", Int),  IfLE ("v1.728.1228",V ("v2.725.1226"),
                                        Ans (Set (1)),
                                        Ans (Set (0))),
     Let (("Ti153.731.1230", Int),  Sub ("sp.400.1071",C 2 ),
     Let (("Tu12.732.1231", Unit),  St ("n.729.1229","stack.399",V ("Ti153.731.1230"),4),
     Let (("Ti155.734.1232", Int),  Sub ("sp.400.1071",C 1 ),
     Let (("sp.400.1294", Int),  Mov ("Ti155.734.1232"),
     Let (("Ti191.680.1421", Int),  Sub ("sp.400.1294",C 1 ),
     Let (("v.681.1422", Int),  Ld ("stack.399",V ("Ti191.680.1421"),4),
     Let (("sp2.683.1423", Int),  Sub ("sp.400.1294",C 1 ),
     Ans (Set 100))))))))))))))))))))))) in
  constfold_arith t_trace1 |> show |> print_endline;
  true
;;

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
