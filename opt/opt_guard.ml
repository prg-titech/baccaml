open Std
open MinCaml
open Asm
open Opt_lib

(* for tracing *)
let rec is_guard_path = function
  | Let (_, e, t) -> is_guard_path_exp e || is_guard_path t
  | Ans (IfEq (x, y, t1, t2))
  | Ans (IfGE (x, y, t1, t2))
  | Ans (IfLE (x, y, t1, t2)) ->
    is_guard_path t1 || is_guard_path t2
  | Ans e -> is_guard_path_exp e

and is_guard_path_exp = function
  | CallDir (Id.L x, _, _) -> String.starts_with x "guard_"
  | _ -> false
;;

let%test "is_guard_path test" =
  let t =
    Let
      ( ("pc.402.1292", Int)
      , Set 16
      , Let
          ( ("bytecode.401.1293", Int)
          , CallDir (L "restore_min_caml_bp", [], [])
          , Let
              ( ("Ti195.686.1424", Int)
              , Add ("pc.402.1292", C 2)
              , Ans
                  (CallDir
                     ( L "guard_tracetj0.844"
                     , [ "stack.399"
                       ; "sp2.683.1423"
                       ; "bytecode.401.1293"
                       ; "Ti195.686.1424"
                       ]
                     , [] )) ) ) )
  in
  is_guard_path t = true
;;

let rec get_insts_inside_guard = function
  | Let ((var, typ), e, t) -> get_insts_inside_guard t
  | Ans (IfEq (x, y, t1, t2)) | Ans (IfLE (x, y, t1, t2)) | Ans (IfGE (x, y, t1, t2)) ->
    if is_guard_path t2
    then t2
    else if is_guard_path t1
    then t1
    else failwith "impossible application."
  | Ans e -> Ans e
;;

let get_vars_inside_guard t =
  Asm.fv (get_insts_inside_guard t)
  |> List.filter (fun var ->
         let re = Str.regexp "stack.\\([a-zA-Z0-9]+\\)\\(\\.[a-ZA-Z0-9]*\\)*" in
         not (Str.string_match re var 0))
;;

let rec get_insts_outside acc vars t =
  match t with
  | Let ((var, typ), e, t) ->
    if List.exists (fun var -> contains var e) vars || List.mem var vars
    then (
      let acc = acc @ [((var, typ), e)] in
      get_insts_outside acc vars t)
    else get_insts_outside acc vars t
  | Ans e -> acc
;;

let rec tree_of_list lst = match lst with
  | (x, e) :: tl -> Let (x, e, tree_of_list tl)
  | [] -> Ans (Nop)
;;

let move_into_guard t =
  let vars_inside_guard = get_vars_inside_guard t in
  let rec move_into_the_guard cand t =
    match t with
    | Let ((var, typ), e, t') ->
      if List.mem var vars_inside_guard
      then move_into_the_guard cand t'
      else Let ((var, typ), e, move_into_the_guard cand t')
    | Ans (IfEq (x, y, t1, t2) as e)
    | Ans (IfLE (x, y, t1, t2) as e)
    | Ans (IfGE (x, y, t1, t2) as e) ->
      if is_guard_path t1
      then (
        let t1' = Asm.concat cand (Id.gentmp Type.Unit, Type.Unit) t1 in
        Ans (e <=> (x, y, t1', t2)))
      else (
        let t2' = Asm.concat cand (Id.gentmp Type.Unit, Type.Unit) t2 in
        Ans (e <=> (x, y, t1, t2')))
    | Ans e -> Ans e
  in
  let insts_outside = get_insts_outside [] vars_inside_guard t
                      |> tree_of_list in
  move_into_the_guard insts_outside t
;;

let%test_module "move guard insts test" =
  (module struct
    let t1 =
      Let (("Ti244.611", Int),  Sub ("sp.400",C 3 ),
      Let (("v.612", Int),  Ld ("stack.399",V "Ti244.611",4),
      Let (("Tu24.613", Unit),  St ("v.612","stack.399",V "sp.400",4),
      Let (("sp.400.848", Int),  Add ("sp.400",C 1 ),
      Let (("Ti333.507.868", Int),  Set (0),
      Let (("Tu32.508.869", Unit),  St ("Ti333.507.868","stack.399",V "sp.400.848",4),
      Let (("Ti149.724.1225", Int),  Add ("sp.400",C 1 ),
      Let (("v2.725.1226", Int),  Mov ("Ti333.507.868"),
      Let (("Ti151.727.1227", Int),  Add ("sp.400",C 0 ),
      Let (("v1.728.1228", Int),  Mov ("v.612"),
      Let (("n.729.1229", Int),  IfLE ("v1.728.1228",V "v2.725.1226",
      Ans (Set (1)),
      Ans (Set (0))),
      Let (("Ti153.731.1230", Int),  Add ("sp.400",C 0 ),
      Let (("Tu12.732.1231", Unit),  St ("n.729.1229","stack.399",V "Ti153.731.1230",4),
      Let (("Ti191.680.1421", Int),  Add ("sp.400",C 0 ),
      Let (("v.681.1422", Int),  Mov ("n.729.1229"),
      Let (("sp2.683.1423", Int),  Add ("sp.400",C 0 ),
      Ans (IfEq ("v.681.1422",C 0 ,
      Let (("sp.400.1517", Int),  Add ("sp.400",C 0 ),
      Let (("Ti244.611.1607", Int),  Add ("sp.400",C (-3) ),
      Let (("v.612.1608", Int),  Ld ("stack.399",V "Ti244.611.1607",4),
      Let (("Tu24.613.1609", Unit),  St ("v.612.1608","stack.399",V "sp.400.1517",4),
      Let (("sp.400.1740", Int),  Add ("sp.400",C 1 ),
      Let (("c.689.1872", Int),  Set (1),
      Let (("Tu15.690.1873", Unit),  St ("c.689.1872","stack.399",V "sp.400.1740",4),
      Let (("Ti85.799.2165", Int),  Add ("sp.400",C 1 ),
      Let (("v2.800.2166", Int),  Mov ("c.689.1872"),
      Let (("Ti87.802.2167", Int),  Add ("sp.400",C 0 ),
      Let (("v1.803.2168", Int),  Mov ("v.612.1608"),
      Let (("Ti89.805.2169", Int),  Add ("sp.400",C 0 ),
      Let (("Ti90.806.2170", Int),  Sub ("v1.803.2168",V "v2.800.2166"),
      Let (("Tu6.807.2171", Unit),  St ("Ti90.806.2170","stack.399",V "Ti89.805.2169",4),
      Let (("sp.400.2186", Int),  Add ("sp.400",C 1 ),
      Let (("Ti340.500.2201", Int),  Add ("sp.400",C 0 ),
      Let (("v.501.2202", Int),  Mov ("Ti90.806.2170"),
      Let (("Tu33.502.2203", Unit),  St ("v.501.2202","stack.399",V "sp.400.2186",4),
      Let (("sp.400.2409", Int),  Add ("sp.400",C 2 ),
      Let (("Ti208.663.2524", Int),  Set (31),
      Let (("Tu21.664.2525", Unit),  St ("Ti208.663.2524","stack.399",V "sp.400.2409",4),
      Let (("Ti210.666.2526", Int),  Add ("sp.400",C 3 ),
      Let (("Ti211.667.2527", Int),  Set (200),
      Let (("Tu20.668.2528", Unit),  St ("Ti211.667.2527","stack.399",V "Ti210.666.2526",4),
      Let (("sp2.670.2529", Int),  Add ("sp.400",C 4 ),
      Ans (CallDir (L "tracetj0.844",["stack.399"; "sp2.670.2529"; ],[]))))))))))))))))))))))))))),
      Let (("pc.402.1292", Int),  Set (16),
      Let (("bytecode.401.1293", Int),  CallDir (L "restore_min_caml_bp",[],[]),
      Let (("Ti195.686.1424", Int),  Add ("pc.402.1292",C 2 ),
      Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp2.683.1423"; "bytecode.401.1293"; "Ti195.686.1424"; ],[])))))))))))))))))))))))
    [@@ocamlformat "disable"]

    let t1_moved =
      Let (("Ti244.611", Int),  Sub ("sp.400",C 3 ),
      Let (("v.612", Int),  Ld ("stack.399",V "Ti244.611",4),
      Let (("Tu24.613", Unit),  St ("v.612","stack.399",V "sp.400",4),
      Let (("sp.400.848", Int),  Add ("sp.400",C 1 ),
      Let (("Ti333.507.868", Int),  Set (0),
      Let (("Tu32.508.869", Unit),  St ("Ti333.507.868","stack.399",V "sp.400.848",4),
      Let (("Ti149.724.1225", Int),  Add ("sp.400",C 1 ),
      Let (("v2.725.1226", Int),  Mov ("Ti333.507.868"),
      Let (("Ti151.727.1227", Int),  Add ("sp.400",C 0 ),
      Let (("v1.728.1228", Int),  Mov ("v.612"),
      Let (("n.729.1229", Int),  IfLE ("v1.728.1228",V "v2.725.1226",
      Ans (Set (1)),
      Ans (Set (0))),
      Let (("Ti153.731.1230", Int),  Add ("sp.400",C 0 ),
      Let (("Tu12.732.1231", Unit),  St ("n.729.1229","stack.399",V "Ti153.731.1230",4),
      Let (("Ti191.680.1421", Int),  Add ("sp.400",C 0 ),
      Let (("v.681.1422", Int),  Mov ("n.729.1229"),
      Ans (IfEq ("v.681.1422",C 0 ,
      Let (("sp.400.1517", Int),  Add ("sp.400",C 0 ),
      Let (("Ti244.611.1607", Int),  Add ("sp.400",C (-3) ),
      Let (("v.612.1608", Int),  Ld ("stack.399",V "Ti244.611.1607",4),
      Let (("Tu24.613.1609", Unit),  St ("v.612.1608","stack.399",V "sp.400.1517",4),
      Let (("sp.400.1740", Int),  Add ("sp.400",C 1 ),
      Let (("c.689.1872", Int),  Set (1),
      Let (("Tu15.690.1873", Unit),  St ("c.689.1872","stack.399",V "sp.400.1740",4),
      Let (("Ti85.799.2165", Int),  Add ("sp.400",C 1 ),
      Let (("v2.800.2166", Int),  Mov ("c.689.1872"),
      Let (("Ti87.802.2167", Int),  Add ("sp.400",C 0 ),
      Let (("v1.803.2168", Int),  Mov ("v.612.1608"),
      Let (("Ti89.805.2169", Int),  Add ("sp.400",C 0 ),
      Let (("Ti90.806.2170", Int),  Sub ("v1.803.2168",V "v2.800.2166"),
      Let (("Tu6.807.2171", Unit),  St ("Ti90.806.2170","stack.399",V "Ti89.805.2169",4),
      Let (("sp.400.2186", Int),  Add ("sp.400",C 1 ),
      Let (("Ti340.500.2201", Int),  Add ("sp.400",C 0 ),
      Let (("v.501.2202", Int),  Mov ("Ti90.806.2170"),
      Let (("Tu33.502.2203", Unit),  St ("v.501.2202","stack.399",V "sp.400.2186",4),
      Let (("sp.400.2409", Int),  Add ("sp.400",C 2 ),
      Let (("Ti208.663.2524", Int),  Set (31),
      Let (("Tu21.664.2525", Unit),  St ("Ti208.663.2524","stack.399",V "sp.400.2409",4),
      Let (("Ti210.666.2526", Int),  Add ("sp.400",C 3 ),
      Let (("Ti211.667.2527", Int),  Set (200),
      Let (("Tu20.668.2528", Unit),  St ("Ti211.667.2527","stack.399",V "Ti210.666.2526",4),
      Let (("sp2.670.2529", Int),  Add ("sp.400",C 4 ),
      Ans (CallDir (L "tracetj0.844",["stack.399"; "sp2.670.2529"; ],[]))))))))))))))))))))))))))),
      Let (("sp2.683.1423", Int),  Add ("sp.400",C 0 ),
      Let (("Tu1", Unit), Nop,
      Let (("pc.402.1292", Int),  Set (16),
      Let (("bytecode.401.1293", Int),  CallDir (L "restore_min_caml_bp",[],[]),
      Let (("Ti195.686.1424", Int),  Add ("pc.402.1292",C 2 ),
      Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp2.683.1423"; "bytecode.401.1293"; "Ti195.686.1424"; ],[]))))))))))))))))))))))))
    [@@ocamlformat "disable"]

    let%test "get_insts_inside_guard test" =
      print_endline "";
      let r1 = get_insts_inside_guard t1 in
      let guard_branch =
        Let (("pc.402.1292", Int),  Set (16),
             Let (("bytecode.401.1293", Int),  CallDir (L "restore_min_caml_bp",[],[]),
                  Let (("Ti195.686.1424", Int),  Add ("pc.402.1292",C 2 ),
                       Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp2.683.1423"; "bytecode.401.1293"; "Ti195.686.1424"; ],[])))))
        [@ocamlformat "disable"]
      in
      r1 = guard_branch
    ;;

    let%test "get_vars_inside_guard test" =
      let r1 = get_vars_inside_guard t1 in
      r1 = [ "sp2.683.1423" ]
    ;;

    let%test "get_insts_outside test" =
      let vars = get_vars_inside_guard t1 in
      let r1 = get_insts_outside [] vars t1 |> tree_of_list in
      r1 = Let (("sp2.683.1423", Type.Int), Add ("sp.400", C 0), Ans Nop)
    ;;

    let%test "move_into_guard test" =
      let r1 = move_into_guard t1 in
      r1 = t1_moved
    ;;
  end)
;;
