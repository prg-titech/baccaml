open Std
open MinCaml
open Asm

let ( |=| ) s1 s2 = String.(get_name s1 = get_name s2)

let rec replace var t =
  match t with
  | Ans e -> (match replace_exp var e with Some exp -> Ans exp | None -> Ans Nop)
  | Let ((x, typ), e, t) ->
    (match replace_exp var e with
    | Some e -> Let ((x, typ), e, replace var t)
    | None -> replace var t)

and replace_exp var exp =
  match exp with
  | Nop | Set _ | SetL _ -> Some exp
  | Mov id | SMov id | Neg id -> if id |=| var then None else Some exp
  | Add (x, V y) ->
    Some
      (match x |=| var, y |=| var with
      | true, true -> Add (var, V var)
      | true, false -> Add (var, V y)
      | false, true -> Add (x, V var)
      | false, false -> Add (x, V y))
  | Add (x, C y) -> Some (if x |=| var then Add (var, C y) else Add (x, C y))
  | Sub (x, V y) ->
    Some
      (match x |=| var, y |=| var with
      | true, true -> Sub (var, V var)
      | true, false -> Sub (var, V y)
      | false, true -> Sub (x, V var)
      | false, false -> Sub (x, V y))
  | Sub (x, C y) -> Some (if x |=| var then Sub (var, C y) else Sub (x, C y))
  | Mul (x, V y) ->
    Some
      (match x |=| var, y |=| var with
      | true, true -> Mul (var, V var)
      | true, false -> Mul (var, V y)
      | false, true -> Mul (x, V var)
      | false, false -> Mul (x, V y))
  | Mul (x, C y) -> Some (if x |=| var then Mul (var, C y) else Mul (x, C y))
  | Div (x, V y) ->
    Some
      (match x |=| var, y |=| var with
      | true, true -> Mul (var, V var)
      | true, false -> Div (var, V y)
      | false, true -> Div (x, V var)
      | false, false -> Div (x, V y))
  | Div (x, C y) -> Some (if x |=| var then Div (var, C y) else Div (x, C y))
  | Mod (x, V y) ->
    Some
      (match x |=| var, y |=| var with
      | true, true -> Mul (var, V var)
      | true, false -> Mod (var, V y)
      | false, true -> Mod (x, V var)
      | false, false -> Mod (x, V y))
  | Mod (x, C y) -> Some (if x |=| var then Mod (var, C y) else Mod (x, C y))
  | Ld (x, V y, z) ->
    Some
      (match x |=| var, y |=| var with
      | true, true -> Ld (var, V var, z)
      | true, false -> Ld (var, V y, z)
      | false, true -> Ld (x, V var, z)
      | false, false -> Ld (x, V y, z))
  | Ld (x, C y, z) -> Some (if x |=| var then Ld (var, C y, z) else Ld (x, C y, z))
  | St (x, y, V z, w) ->
    Some
      (match y |=| var, z |=| var with
      | true, true -> St (x, var, V var, w)
      | true, false -> St (x, var, V z, w)
      | false, true -> St (x, y, V var, w)
      | false, false -> St (x, y, V z, w))
  | St (x, y, C z, w) ->
    Some (if y |=| var then St (x, var, C z, w) else St (x, y, C z, z))
  | IfEq (x, V y, t1, t2) ->
    Some
      (match x |=| var, y |=| var with
      | true, true -> IfEq (var, V var, replace var t1, replace var t2)
      | true, false -> IfEq (var, V y, replace var t1, replace var t2)
      | false, true -> IfEq (x, V var, replace var t1, replace var t2)
      | false, false -> IfEq (x, V y, replace var t1, replace var t2))
  | IfEq (x, C y, t1, t2) ->
    Some
      (if x |=| var
      then IfEq (var, C y, replace var t1, replace var t2)
      else IfEq (x, C y, replace var t1, replace var t2))
  | IfLE (x, V y, t1, t2) ->
    Some
      (match x |=| var, y |=| var with
      | true, true -> IfLE (var, V var, replace var t1, replace var t2)
      | true, false -> IfLE (var, V y, replace var t1, replace var t2)
      | false, true -> IfLE (x, V var, replace var t1, replace var t2)
      | false, false -> IfLE (x, V y, replace var t1, replace var t2))
  | IfLE (x, C y, t1, t2) ->
    Some
      (if x |=| var
      then IfLE (var, C y, replace var t1, replace var t2)
      else IfLE (x, C y, replace var t1, replace var t2))
  | IfGE (x, V y, t1, t2) ->
    Some
      (match x |=| var, y |=| var with
      | true, true -> IfGE (var, V var, replace var t1, replace var t2)
      | true, false -> IfGE (var, V y, replace var t1, replace var t2)
      | false, true -> IfGE (x, V var, replace var t1, replace var t2)
      | false, false -> IfGE (x, V y, replace var t1, replace var t2))
  | IfGE (x, C y, t1, t2) ->
    Some
      (if x |=| var
      then IfGE (var, C y, replace var t1, replace var t2)
      else IfGE (x, C y, replace var t1, replace var t2))
  | _ -> failwith "un matched pattern."
;;

let%test _ =
  let open Type in
  let t = Let (("y.1", Int), Add ("x.0", C (1)),
               Let (("stack.379.209", Unit), Mov ("stack.379"),
                    Let (("v.11", Int), Ld ("stack.379.209", V "y.1", 4),
                         Ans (IfEq ("x.0", C (1),
                                    Ans (Mov "y.1"),
                                    Let (("z.10", Int), Add ("x.0", V ("y.1")),
                                         Ans (Mov ("z.10")))))))) in
  let res = replace "stack.379" t in
  let expected =
    (Asm.Let (("y.1", Type.Int), (Asm.Add ("x.0", (Asm.C 1))),
              (Asm.Let (("v.11", Type.Int), (Asm.Ld ("stack.379", (Asm.V "y.1"), 4)),
                        (Asm.Ans
                           (Asm.IfEq ("x.0", (Asm.C 1), (Asm.Ans (Asm.Mov "y.1")),
                                      (Asm.Let (("z.10", Type.Int), (Asm.Add ("x.0", (Asm.V "y.1"))),
                                                (Asm.Ans (Asm.Mov "z.10"))))
                                     )))
                       ))
             )) in
  expected = res
;;
