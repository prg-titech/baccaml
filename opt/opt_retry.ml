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
  | Let ((var, typ), e, t) ->
    Let ((var, typ), e, rename_guard { pc; bname } t)
  | Ans (CallDir (Id.L name, args, fargs))
    when String.starts_with name "guard_" ->
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

let embed_bridge' bridge_args bridge_body e =
  let rec zip l1 l2 =
    match l1, l2 with
    | hd1 :: tl1, hd2 :: tl2
      when String.(get_name hd1 = get_name hd2)
      -> (hd1, hd2) :: zip tl1 tl2
    | hd1 :: tl1, hd2 :: tl2 ->
      failwith "different name"
    | [], [] -> []
    | _ -> []
  in
  let rec insert cont = function
    | (bridge_arg, arg) :: tl ->
      Let ((bridge_arg, Type.Int), Mov arg, insert cont tl)
    | [] -> cont
  in
  match e with
  | CallDir (Id.L name, args, fargs) ->
    zip bridge_args args |> insert bridge_body
  | _ -> failwith (Printf.sp "%s is not CallDir" (Asm.show_exp e))
;;

let rec embed_bridge ~bargs:bridge_args ~bbody:bridge_body = function
  | Let (x, CallDir (Id.L "min_caml_guard_occur_at", _, _), t) ->
    embed_bridge ~bargs:bridge_args ~bbody:bridge_body t
  | Let (x, e, t) -> Let (x, e, embed_bridge bridge_args bridge_body t)
  | Ans (CallDir (Id.L name, args, fargs) as e)
    when String.starts_with name "guard_" ->
    embed_bridge' bridge_args bridge_body e
  | Ans e -> Ans e
;;

let embed { pc; bname } { name; args; fargs; body; ret } bridge
  =
  let rec loop = function
    | Let (x, e, t) -> Let (x, e, loop t)
    | Ans (IfEq (x, y, t1, t2) as e)
    | Ans (IfLE (x, y, t1, t2) as e)
    | Ans (IfGE (x, y, t1, t2) as e) ->
      if Opt_guard.is_guard_path t1 && has_guard_at pc t1
      then
        let { args= bargs'; body= bbody' } = Renaming.rename_fundef bridge in
        Ans (e <=> (x, y, embed_bridge bargs' bbody' t1, t2))
      else if Opt_guard.is_guard_path t2 && has_guard_at pc t1
      then
        let { args= bargs'; body= bbody' } = Renaming.rename_fundef bridge in
        Ans (e <=> (x, y, t1, embed_bridge bargs' bbody' t2))
      else Ans (e <=> (x, y, loop t1, loop t2))
    | Ans e -> Ans e
  in
  { name; args; fargs; body= loop body; ret }
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
       Let (("dummy", Type.Unit), GuardAt (18),
       Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp.400"; "bytecode.401.1293"; "Ti195.686.1424"; ],[]))))))))))))
      ; ret= Type.Int; }
    [@@ocamlformat "disable"]

    let rec extract_calldirs = function
      | Ans (CallDir (id_l, args, fargs)) ->
        [ CallDir (id_l, args, fargs) ]
      | Ans (IfEq (_, _, t1, t2))
      | Ans (IfLE (_, _, t1, t2))
      | Ans (IfGE (_, _, t1, t2)) ->
        extract_calldirs t1 @ extract_calldirs t2
      | Ans e -> []
      | Let (_, CallDir (id_l, args, fargs), t) ->
        [ CallDir (id_l, args, fargs) ] @ extract_calldirs t
      | Let (_, _, t) -> extract_calldirs t
    ;;

    let%test "test rename_guard" =
      print_endline "\027[32m[TEST] rename_guard test\027[0m";
      let rg_env = { pc = 18; bname = "renamed_tracetj1.999" } in
      let res = rename rg_env trace_sum in
      (* print_fundef res; print_newline (); *)
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

    let%test "test embed" =
      print_endline "\027[32m[TEST] embed test\027[0m";
      let parent =
         { name= L "tracetj0.844"; args= ["stack.399"; "sp.400"; ]; fargs= []; body=
           Let (("Ti244.611", Int),  Sub ("sp.400",C 4 ),
           Let (("v.612", Int),  Ld ("stack.399",V "Ti244.611",4),
           Let (("c.689.980", Int),  Set (8192),
           Let (("n.729.1229", Int),  IfLE ("v.612",V "c.689.980",
           Let (("Tu848", Unit),  BranchingAt (84),
           Ans (Set (1))),
           Ans (Set (0))),
           Ans (IfEq ("n.729.1229",C 0 ,
           Let (("Tu847", Unit),  BranchingAt (84),
           Let (("bytecode.401.1293", Int),  CallDir (L "restore_min_caml_bp",[],[]),
           Let (("addr.678.1420", Int),  Set (91),
           Let (("Tu846", Unit),  GuardAt (91),
           Let (("Tu845", Unit),  CallDir (L "min_caml_guard_occur_at",["stack.399"; "sp.400"; "bytecode.401.1293"; "addr.678.1420"; ],[]),
           Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp.400"; "bytecode.401.1293"; "addr.678.1420"; ],[]))))))),
           Let (("c.689.1649", Int),  Set (-1),
           Let (("Tu15.690.1650", Unit),  St ("c.689.1649","stack.399",V "sp.400",4),
           Let (("Ti244.611.2053", Int),  Sub ("sp.400",C 5 ),
           Let (("v.612.2054", Int),  Ld ("stack.399",V "Ti244.611.2053",4),
           Let (("Ti244.611.2276", Int),  Sub ("sp.400",C 4 ),
           Let (("v.612.2277", Int),  Ld ("stack.399",V "Ti244.611.2276",4),
           Let (("arr.568.2471", Array (Int)),  CallDir (L "cast_fIAI.390",["v.612.2054"; ],[]),
           Let (("Tu29.572.2474", Unit),  St ("c.689.1649","arr.568.2471",V "v.612.2277",4),
           Let (("Ti282.574.2475", Int),  Add ("sp.400",C 1 ),
           Let (("Ti283.575.2476", Int),  CallDir (L "cast_fAII.388",["arr.568.2471"; ],[]),
           Let (("Tu28.576.2477", Unit),  St ("Ti283.575.2476","stack.399",V "Ti282.574.2475",4),
           Let (("Ti244.611.2722", Int),  Sub ("sp.400",C 4 ),
           Let (("v.612.2723", Int),  Ld ("stack.399",V "Ti244.611.2722",4),
           Let (("Ti244.611.2945", Int),  Sub ("sp.400",C 3 ),
           Let (("v.612.2946", Int),  Ld ("stack.399",V "Ti244.611.2945",4),
           Let (("Ti77.819.3293", Int),  Add ("sp.400",C 2 ),
           Let (("Ti78.820.3294", Int),  Add ("v.612.2723",V "v.612.2946"),
           Let (("Tu5.821.3295", Unit),  St ("Ti78.820.3294","stack.399",V "Ti77.819.3293",4),
           Let (("sp.400.3301", Int),  Add ("sp.400",C 3 ),
           Let (("Ti244.611.3391", Int),  Sub ("sp.400",C 5 ),
           Let (("v.612.3392", Int),  Ld ("stack.399",V "Ti244.611.3391",4),
           Let (("Tu24.613.3393", Unit),  St ("v.612.3392","stack.399",V "sp.400.3301",4),
           Let (("sp.400.3524", Int),  Add ("sp.400",C 4 ),
           Let (("Tu24.613.3616", Unit),  St ("Ti78.820.3294","stack.399",V "sp.400.3524",4),
           Let (("sp.400.3747", Int),  Add ("sp.400",C 5 ),
           Let (("Ti244.611.3837", Int),  Sub ("sp.400",C 3 ),
           Let (("v.612.3838", Int),  Ld ("stack.399",V "Ti244.611.3837",4),
           Let (("Tu24.613.3839", Unit),  St ("v.612.3838","stack.399",V "sp.400.3747",4),
           Let (("Ti310.535.4008", Int),  Sub ("sp.400",C 1 ),
           Let (("ret.536.4009", Int),  Ld ("stack.399",V "Ti310.535.4008",4),
           Let (("old_base.541.4013", Int),  Sub ("sp.400",C 5 ),
           Let (("new_base.542.4014", Int),  Add ("sp.400",C 3 ),
           Let (("n.531.4005", Int),  Set (3),
           Let (("Ti315.543.4015", Int),  Set (0),
           Let (("sp.400.4193", Int),  CallDir (L "frame_reset.392",["stack.399"; "old_base.541.4013"; "new_base.542.4014"; "ret.536.4009"; "n.531.4005"; "Ti315.543.4015"; ],[]),
           Ans (CallDir (L "tracetj0.844",["stack.399"; "sp.400.4193"; ],[])))))))))))))))))))))))))))))))))))))))))))
         ; ret= Int;}
         [@@ocamlformat "disable"]
      in
      let bridge =
            { name= L "tracetj2.934"; args= ["stack.399.2275"; "sp.400.2274"; ]; fargs= []; body=
              Let (("Ti244.611.2276", Int),  Sub ("sp.400.2274",C 4 ),
              Let (("v.612.2277", Int),  Ld ("stack.399.2275",V "Ti244.611.2276",4),
              Let (("Ti244.611.1028.2278", Int),  Sub ("sp.400.2274",C 4 ),
              Let (("v.612.1029.2279", Int),  Ld ("stack.399.2275",V "Ti244.611.1028.2278",4),
              Let (("Ti78.820.1377.2280", Int),  Add ("v.612.2277",V "v.612.1029.2279"),
              Let (("Tu5.821.1378.2281", Unit),  St ("Ti78.820.1377.2280","stack.399.2275",V "sp.400.2274",4),
              Let (("sp.400.1384.2282", Int),  Add ("sp.400.2274",C 1 ),
              Let (("Ti244.611.1474.2283", Int),  Sub ("sp.400.2274",C 5 ),
              Let (("v.612.1475.2284", Int),  Ld ("stack.399.2275",V "Ti244.611.1474.2283",4),
              Let (("Tu24.613.1476.2285", Unit),  St ("v.612.1475.2284","stack.399.2275",V "sp.400.1384.2282",4),
              Let (("sp.400.1607.2286", Int),  Add ("sp.400.2274",C 2 ),
              Let (("Tu24.613.1699.2287", Unit),  St ("Ti78.820.1377.2280","stack.399.2275",V "sp.400.1607.2286",4),
              Let (("sp.400.1830.2288", Int),  Add ("sp.400.2274",C 3 ),
              Let (("Ti244.611.1920.2289", Int),  Sub ("sp.400.2274",C 4 ),
              Let (("v.612.1921.2290", Int),  Ld ("stack.399.2275",V "Ti244.611.1920.2289",4),
              Let (("Tu24.613.1922.2291", Unit),  St ("v.612.1921.2290","stack.399.2275",V "sp.400.1830.2288",4),
              Let (("sp.400.2053.2292", Int),  Add ("sp.400.2274",C 4 ),
              Let (("Ti208.663.2168.2293", Int),  Set (146),
              Let (("Tu21.664.2169.2294", Unit),  St ("Ti208.663.2168.2293","stack.399.2275",V "sp.400.2053.2292",4),
              Let (("Ti210.666.2170.2295", Int),  Add ("sp.400.2274",C 5 ),
              Let (("Ti211.667.2171.2296", Int),  Set (200),
              Let (("Tu20.668.2172.2297", Unit),  St ("Ti211.667.2171.2296","stack.399.2275",V "Ti210.666.2170.2295",4),
              Let (("sp2.670.2173.2298", Int),  Add ("sp.400.2274",C 6 ),
              Ans (CallDir (L "tracetj0.844",["stack.399.2275"; "sp2.670.2173.2298"; ],[])))))))))))))))))))))))))
            ; ret= Int; }
      in
      let res = embed { pc= 91; bname = "tracetj2.934" } parent bridge in
      print_fundef res; print_newline ();
      res |> Simm.h |> RegAlloc.h |> Emit.h stdout;
      true
  end)
;;
