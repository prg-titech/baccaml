open MinCaml
open Asm

type cond =
  | Eq of Id.t * Asm.id_or_imm
  | LE of Id.t * Asm.id_or_imm
  | GE of Id.t * Asm.id_or_imm
[@@deriving show]

type if_body = IfBody of t_opt list * t_opt list [@@deriving show]

and e_opt =
  | E of exp
  | If of cond * if_body
[@@deriving show]

and t_opt =
  | L of (Id.t * Type.t) * e_opt
  | A of e_opt
[@@deriving show]

let rec t_opt_of_t_asm (t_asm : Asm.t) =
  match t_asm with
  | Let (x', IfEq (x, y, t1, t2), t) ->
    L (x', If (Eq (x, y), IfBody (t_opt_of_t_asm t1, t_opt_of_t_asm t2)))
    :: t_opt_of_t_asm t
  | Let (x', IfLE (x, y, t1, t2), t) ->
    L (x', If (LE (x, y), IfBody (t_opt_of_t_asm t1, t_opt_of_t_asm t2)))
    :: t_opt_of_t_asm t
  | Let (x', IfGE (x, y, t1, t2), t) ->
    L (x', If (GE (x, y), IfBody (t_opt_of_t_asm t1, t_opt_of_t_asm t2)))
    :: t_opt_of_t_asm t
  | Let (x, e, t) -> L (x, E e) :: t_opt_of_t_asm t
  | Ans (IfEq (x, y, t1, t2)) ->
    [ A (If (Eq (x, y), IfBody (t_opt_of_t_asm t1, t_opt_of_t_asm t2))) ]
  | Ans (IfLE (x, y, t1, t2)) ->
    [ A (If (LE (x, y), IfBody (t_opt_of_t_asm t1, t_opt_of_t_asm t2))) ]
  | Ans (IfGE (x, y, t1, t2)) ->
    [ A (If (GE (x, y), IfBody (t_opt_of_t_asm t1, t_opt_of_t_asm t2))) ]
  | Ans e -> [ A (E e) ]
;;

let%test "t_opt_of_t_asm test1" =
  let open Type in
  let t =
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
                  , Let
                      ( ("Ti246.615", Int)
                      , Add ("sp.400", C 1)
                      , Ans (IfEq ("Ti246.615", C 100, Ans (Set 100), Ans (Set (-200))))
                      ) ) ) ) )
  in
  let expected =
    [ L (("Ti242.609", Int), E (Sub ("sp.400", C 2)))
    ; L (("Ti244.611", Int), E (Sub ("Ti242.609", C 1)))
    ; L (("v.612", Int), E (Ld ("stack.399", V "Ti244.611", 4)))
    ; L (("Tu24.613", Unit), E (St ("v.612", "stack.399", V "sp.400", 4)))
    ; L (("Ti246.615", Int), E (Add ("sp.400", C 1)))
    ; A
        (If (Eq ("Ti246.615", C 100), IfBody ([ A (E (Set 100)) ], [ A (E (Set (-200))) ])))
    ]
  in
  t_opt_of_t_asm t = expected
;;

(* exp1 appears after exp2 *)
let rec specialize_arith exp1 exp2 =
  match exp1, exp2 with
  | Add (x, C n), Add (y, C m) -> `Specialized (Add (x, C (n + m)))
  | Sub (x, C n), Sub (y, C m) -> `Specialized (Sub (x, C (n + m)))
  | Add (x, C n), Sub (y, C m) -> `Specialized (Add (x, C (n - m)))
  | Sub (x, C n), Add (y, C m) -> `Specialized (Sub (x, C (n - m)))
  | _ -> `Not_specialized (exp1, exp2)
;;

(* remove an expression 'exp' from 'acc' *)
let rec remove_from_acc exp = function
  | [] -> []
  | L (x, E exp') :: tl ->
    if exp' = exp then tl else L (x, E exp') :: remove_from_acc exp tl
  | L (x, If (cond, IfBody (t1, t2))) :: tl ->
    L (x, If (cond, IfBody (remove_from_acc exp t1, remove_from_acc exp t2)))
    :: remove_from_acc exp tl
  | A (If (cond, IfBody (t1, t2))) :: tl ->
    A (If (cond, IfBody (remove_from_acc exp t1, remove_from_acc exp t2)))
    :: remove_from_acc exp tl
  | A (E exp') :: tl ->
    if exp' = exp then remove_from_acc exp tl else A (E exp') :: remove_from_acc exp tl
;;

let rec specialize_and_remove env acc (var, typ) x exp =
  let exp' =
    try M.find x env with
    | Not_found ->
      Printf.eprintf "%s is not found." x;
      raise Not_found
  in
  match specialize_arith exp exp' with
  | `Specialized exp -> L ((var, typ), E exp) :: remove_from_acc exp' acc
  | `Not_specialized _ -> acc
;;

(* store in-progress states in "acc" *)
(* if it can be specialized, remove the "exp" from "acc" *)
let rec constfold_arith (t_opt : t_opt list) =
  let rec constfold_arith' env acc = function
    | [] -> acc
    | L ((var, typ), E exp') :: tl ->
      (match exp' with
      | (Add (x, C n) | Sub (x, C n)) when M.mem x env ->
        let acc = specialize_and_remove env acc (var, typ) x exp' in
        constfold_arith' env acc tl
      | _ ->
        let env = M.add var exp' env in
        let acc = L ((var, typ), E exp') :: acc in
        constfold_arith' env acc tl)
    | A (E exp') :: tl ->
      let acc = A (E exp') :: acc in
      constfold_arith' env acc tl
  in
  let env = M.empty in
  constfold_arith' env [] t_opt
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
                  , Let (("Ti246.615", Int), Add ("sp.400", C 1), Ans (Set (-100))) ) ) )
      )
  in
  let t_opt_wo_if = t_opt_of_t_asm t_wo_if in
  let result = constfold_arith t_opt_wo_if |> List.rev in
  result |> List.iter (fun t_opt -> Printf.printf "%s;\n" (show_t_opt t_opt));
  true
;;

let contains2 var (id_t, id_or_imm) =
  let open Asm in
  var = id_t || match id_or_imm with C n -> false | V x -> var = x
;;

let contains3 var (id_t1, id_t2, id_or_imm) =
  let open Asm in
  var = id_t1 || var = id_t2 || match id_or_imm with C n -> false | V x -> var = x
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
