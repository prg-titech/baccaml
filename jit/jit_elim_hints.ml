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

let rename_interp ({name= Id.L (name'); args; fargs; body; ret} as fundef) =
  let rec rename = function
    | Ans (exp) -> Ans (rename' exp)
    | Let (x, exp, t) -> Let (x, rename' exp, rename t)
  and rename' = function
    | CallDir (id_l, args, fargs) when let open Id in id_l =||= "interp" ->
      CallDir (Id.L ("interp_no_hints"), args, fargs)
    | IfEq (x, y, t1, t2) -> IfEq (x, y, rename t1, rename t2)
    | IfLE (x, y, t1, t2) -> IfLE (x, y, rename t1, rename t2)
    | IfGE (x, y, t1, t2) -> IfGE (x, y, rename t1, rename t2)
    | SIfEq (x, y, t1, t2) -> SIfEq (x, y, rename t1, rename t2)
    | SIfLE (x, y, t1, t2) -> SIfLE (x, y, rename t1, rename t2)
    | SIfGE (x, y, t1, t2) -> SIfGE (x, y, rename t1, rename t2)
    | exp -> exp
  in
  if name' = "interp"
  then {name= Id.L ("interp_no_hints"); args; fargs; body= rename body; ret}
  else fundef


let elim_hints_fundef ({name; args; fargs; body; ret}) =
  {name; args; fargs; body= elim_hints body; ret}


let elim_hints_and_rename fundefs =
  let open Id in
  match fundefs |> List.find_opt (fun fundef -> fundef.name =||= "interp") with
  | Some interp -> (elim_hints_fundef interp |> rename_interp) :: fundefs
  | None -> fundefs


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
