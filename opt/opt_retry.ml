open Std
open MinCaml
open Jit
open Opt_lib

type rename_guard_env =
  { pc : int
  ; tname : string
  }

let is_pc x =
  let re = Str.regexp "^pc\\.[a-zA-Z0-9\\.]*" in
  Str.string_match re x 0
;;

let rec rename_guard { pc; tname } env =
  let open Asm in
  function
  | Let ((var, typ), Set n, t) when is_pc var ->
    let env = M.add var n env in
    Let ((var, typ), Set n, rename_guard { pc; tname } env t)
  | Let ((var, typ), Add (x, C y), t) when is_pc x ->
    let pc_v = M.find x env in
    let env = M.add var (pc_v + y) env in
    Let ((var, typ), Add (x, C y), rename_guard { pc; tname } env t)
  | Let ((var, typ), Sub (x, C y), t) when is_pc x ->
    let pc_v = M.find x env in
    let env = M.add var (pc_v - y) env in
    Let ((var, typ), Sub (x, C y), rename_guard { pc; tname } env t)
  | Let ((var, typ), e, t) ->
    Let ((var, typ), e, rename_guard { pc; tname } env t)
  | Ans (CallDir (Id.L name, args, fargs) as e) ->
    Option.(
      bind
        (List.find_opt (fun arg -> M.mem arg env) args)
        (fun pc_arg ->
          bind (M.find_opt pc_arg env) (fun pc_v ->
              if pc_v = pc
              then
                Let
                  ( (Id.gentmp Type.Unit, Type.Unit)
                  , Comment ("guard_pc." ^ string_of_int pc_v)
                  , Ans (CallDir (Id.L tname, args, fargs)) )
                |> some
              else none))
      |> value ~default:(Ans e))
;;

(** TODO: Check the value of pc that a guard instruction has, and exec
    `rename_guard' at that point **)
let rename rg_env trace_fundef =
  let open Asm in
  let rec aux rg_env = function
    | Let (x, e, t) -> Let (x, e, aux rg_env t)
    | Ans (IfEq (x, y, t1, t2) as e)
    | Ans (IfLE (x, y, t1, t2) as e)
    | Ans (IfGE (x, y, t1, t2) as e) ->
      if Opt_guard.is_guard_path t2
      then Ans (e <=> (x, y, t1, rename_guard rg_env M.empty t2))
      else Ans (e <=> (x, y, rename_guard rg_env M.empty t1, t2))
    | Ans e -> Ans e
  in
  let { name; args; fargs; body; ret } = trace_fundef in
  let renamed_body = aux rg_env body in
  create_fundef ~name ~args ~fargs ~body:renamed_body ~ret
  |> Renaming.rename_fundef
;;

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
       Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp.400"; "bytecode.401.1293"; "Ti195.686.1424"; ],[])))))))))))
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
      let rg_env = { pc = 18; tname = "renamed_tracetj1.999" } in
      let res = rename rg_env trace_sum in
      print_t res.body;
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
