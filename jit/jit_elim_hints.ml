open Std
open MinCaml
open Asm

let hints =
  ["min_caml_jit_merge_point";
   "min_caml_can_enter_jit";]

let contain_hints (Id.L x) = List.mem (String.get_name x) hints

let rec elim_hints = function
  | Ans (e) -> Ans (elim_hints' e)
  | Let (x, CallDir (id_l, args, fargs), t) when contain_hints id_l -> elim_hints t
  | Let (x, e, t) -> Let (x, e, elim_hints t)

and elim_hints' = function
  | IfEq (x, y, t1, t2) -> IfEq (x, y, elim_hints t1, elim_hints t2)
  | IfLE (x, y, t1, t2) -> IfLE (x, y, elim_hints t1, elim_hints t2)
  | IfGE (x, y, t1, t2) -> IfGE (x, y, elim_hints t1, elim_hints t2)
  | SIfEq (x, y, t1, t2) -> SIfEq (x, y, elim_hints t1, elim_hints t2)
  | SIfLE (x, y, t1, t2) -> SIfLE (x, y, elim_hints t1, elim_hints t2)
  | SIfGE (x, y, t1, t2) -> SIfGE (x, y, elim_hints t1, elim_hints t2)
  | exp -> exp

let elim_hints_fundef ({name; args; fargs; body; ret}) =
  {name; args; fargs; body= elim_hints body; ret}

let%test "test_jit_elim_hint" =
  let testee =
    let open Type in
    Let (("x.1", Int), CallDir (Id.L ("min_caml_jit_merge_point.1"), [], []),
         Let (("y.2", Int), Set (47),
              Let (("z.3", Int), Add ("y.2", C (93)),
                   Ans (Mov "z.3"))))
  and expected =
    Let (("y.2", Int), Set (47),
         Let (("z.3", Int), Add ("y.2", C (93)),
              Ans (Mov "z.3")))
  in elim_hints testee = expected

let%test "test_jit_elim_hint_w_if" =
  let testee =
    let open Type in
    Ans (IfEq ("cond.0", C (100),
               Let (("x.1", Int), CallDir (Id.L ("min_caml_jit_merge_point.1"), [], []),
                    Let (("y.2", Int), Set (47),
                         Let (("z.3", Int), Add ("y.2", C (93)),
                              Ans (Mov "z.3")))),
               Ans (Set (-1000))
              ))
  and expected =
    Ans (IfEq ("cond.0", C (100),
               Let (("y.2", Int), Set (47),
                    Let (("z.3", Int), Add ("y.2", C (93)),
                         Ans (Mov "z.3"))),
               Ans (Set (-1000))
              ))
  in elim_hints testee = expected
