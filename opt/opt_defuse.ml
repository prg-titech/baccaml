open Std
open MinCaml
open Asm

type cond =
  | Eq of Id.t * Asm.id_or_imm
  | LE of Id.t * Asm.id_or_imm
  | GE of Id.t * Asm.id_or_imm

type if_body = IfBody of t_opt list * t_opt list

and e_opt =
  | E of exp
  | If of cond * if_body

and t_opt =
  | L of (Id.t * Type.t) * e_opt
  | A of e_opt

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
