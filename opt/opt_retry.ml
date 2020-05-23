open Std
open MinCaml
open Jit
open Opt_lib
open Asm

type rename_guard_env =
  { pc : int
  ; bname : string
  }

let is_pc x =
  Str.(
    let re_pc = regexp "^pc\\.[a-zA-Z0-9\\.]*" in
    let re_addr = regexp "^addr\\.[a-zA-Z0-9\\.]*" in
    string_match re_pc x 0 || string_match re_addr x 0)
;;

let rec has_guard_at guard_pc = function
  | Let (x, GuardAt n, t) -> n = guard_pc
  | Let (x, e, t) -> has_guard_at guard_pc t
  | Ans e -> false
;;

let rec rename_guard ({ pc; bname } as rg_env) =
  let open Asm in
  function
  | Let (x, CallDir (Id.L "min_caml_guard_occur_at", _, _), t) ->
    rename_guard rg_env t
  | Let ((var, typ), e, t) -> Let ((var, typ), e, rename_guard { pc; bname } t)
  | Ans (CallDir (Id.L name, args, fargs)) when String.starts_with name "guard_"
    ->
    Ans (CallDir (Id.L bname, args, fargs))
  | Ans e -> Ans e
;;

(** TODO: Check the value of pc that a guard instruction has, and exec
    `rename_guard' at that point **)
let rename ({ pc; bname } as rg_env) trace_fundef =
  let rec aux rg_env = function
    | Let (x, e, t) -> Let (x, e, aux rg_env t)
    | Ans (IfEq (x, y, t1, t2) as e)
    | Ans (IfLE (x, y, t1, t2) as e)
    | Ans (IfGE (x, y, t1, t2) as e) ->
      if Opt_guard.is_guard_path t1 && has_guard_at pc t1
      then Ans (e <=> (x, y, rename_guard rg_env t1, t2))
      else if Opt_guard.is_guard_path t2 && has_guard_at pc t2
      then Ans (e <=> (x, y, t1, rename_guard rg_env t2))
      else Ans (e <=> (x, y, aux rg_env t1, aux rg_env t2))
    | Ans e -> Ans e
  in
  let { name; args; fargs; body; ret } = trace_fundef in
  let renamed_body = aux rg_env body in
  create_fundef ~name ~args ~fargs ~body:renamed_body ~ret
;;

(* |> Renaming.rename_fundef *)

let%test_module _ =
  (module struct
    open Asm

    let trace_sum =
      { name= L "tracetj0.844"
      ; args= [ "stack.399"; "sp.400"; ]
      ; fargs= []
      ; body=
       Let (("Ti244.611", Int),  Sub ("sp.400",C 3 ),
       Let (("v.612", Int),  Ld ("stack.399",V "Ti244.611",4),
       Let (("Ti333.507.868", Int),  Set (0),
       Let (("n.729.1229", Int),  IfLE ("v.612",V "Ti333.507.868",
       Ans (Set (1)),
       Ans (Set (0))),
       Ans (IfEq ("n.729.1229",C 0 ,
       Let (("Ti244.611.1607", Int),  Sub ("sp.400",C 3 ),
       Let (("v.612.1608", Int),  Ld ("stack.399",V "Ti244.611.1607",4),
       Let (("c.689.1872", Int),  Set (1),
       Let (("Ti90.806.2170", Int),  Sub ("v.612.1608",V "c.689.1872"),
       Let (("Tu6.807.2171", Unit),  St ("Ti90.806.2170","stack.399",V "sp.400",4),
       Let (("sp.400.2186", Int),  Add ("sp.400",C 1 ),
       Let (("Tu33.502.2203", Unit),  St ("Ti90.806.2170","stack.399",V "sp.400.2186",4),
       Let (("sp.400.2409", Int),  Add ("sp.400",C 2 ),
       Let (("Ti208.663.2524", Int),  Set (31),
       Let (("Tu21.664.2525", Unit),  St ("Ti208.663.2524","stack.399",V "sp.400.2409",4),
       Let (("Ti210.666.2526", Int),  Add ("sp.400",C 3 ),
       Let (("Ti211.667.2527", Int),  Set (200),
       Let (("Tu20.668.2528", Unit),  St ("Ti211.667.2527","stack.399",V "Ti210.666.2526",4),
       Let (("sp2.670.2529", Int),  Add ("sp.400",C 4 ),
       Ans (CallDir (L "tracetj0.844",["stack.399"; "sp2.670.2529"; ],[])))))))))))))))),
       Let (("pc.402.1292", Int),  Set (16),
       Let (("bytecode.401.1293", Int),  CallDir (L "restore_min_caml_bp",[],[]),
       Let (("Ti195.686.1424", Int),  Add ("pc.402.1292",C 2 ),
       Let (("dummy", Type.Unit), GuardAt (18),
       Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp.400"; "bytecode.401.1293"; "Ti195.686.1424"; ],[]))))))))))))
      ; ret= Type.Int; }
    [@@ocamlformat "disable"]

    let rec extract_calldirs = function
      | Ans (CallDir (id_l, args, fargs)) -> [ CallDir (id_l, args, fargs) ]
      | Ans (IfEq (_, _, t1, t2))
      | Ans (IfLE (_, _, t1, t2))
      | Ans (IfGE (_, _, t1, t2)) ->
        extract_calldirs t1 @ extract_calldirs t2
      | Ans e -> []
      | Let (_, CallDir (id_l, args, fargs), t) ->
        [ CallDir (id_l, args, fargs) ] @ extract_calldirs t
      | Let (_, _, t) -> extract_calldirs t
    ;;

    let%test _ =
      print_endline "\027[32m[TEST] rename_guard test\027[0m";
      let rg_env = { pc = 18; bname = "renamed_tracetj1.999" } in
      let res = rename rg_env trace_sum in
      print_fundef res;
      print_newline ();
      let _ =
        assert (
          List.exists
            (function
              | CallDir (Id.L x, args, fargs) -> x = "renamed_tracetj1.999"
              | _ -> false)
            (extract_calldirs res.body))
      in
      res != trace_sum
    ;;
  end)
;;
