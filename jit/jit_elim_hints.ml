open Std
open MinCaml
open Asm

let elim_hints =
  ["min_caml_jit_merge_point";
   "min_caml_can_enter_jit";]

let rec g = function
  | Ans (e) ->
    begin match g' e with
      | `Elim -> Ans (Nop)
      | `Not_elim e -> Ans (e)
    end
  | Let ((x, typ), e, t) ->
    begin match g' e with
      | `Elim -> g t
      | `Not_elim e -> Let ((x, typ), e, g t)
    end

and g' = function
  | CallDir (Id.L x, args, fargs) as exp ->
    if List.mem (String.get_name x) elim_hints
    then `Elim
    else `Not_elim (exp)
  | exp -> `Not_elim (exp)

let h ({name; args; fargs; body; ret}) =
  {name; args; fargs; body= g body; ret}

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
  in g testee = expected
