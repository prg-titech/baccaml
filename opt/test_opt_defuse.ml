open MinCaml
open Asm
open Opt_defuse

let%test_module "constfold test" = (module struct
  let t_trace1 =
    Let (("Ti242.609", Int),  Sub ("sp.400",C 2 ),
    Let (("Ti244.611", Int),  Sub ("Ti242.609",C 1 ),
    Let (("v.612", Int),  Ld ("stack.399",V "Ti244.611",4),
    Let (("Tu24.613", Unit),  St ("v.612","stack.399",V "sp.400",4),
    Let (("Ti246.615", Int),  Add ("sp.400",C 1 ),
    Let (("sp.400.848", Int),  Mov ("Ti246.615"),
    Let (("Ti333.507.868", Int),  Set (0),
    Let (("Tu32.508.869", Unit),  St ("Ti333.507.868","stack.399",V "sp.400.848",4),
    Let (("Ti335.510.870", Int),  Add ("sp.400.848",C 1 ),
    Let (("sp.400.1071", Int),  Mov ("Ti335.510.870"),
    Let (("Ti149.724.1225", Int),  Sub ("sp.400.1071",C 1 ),
    Let (("v2.725.1226", Int),  Ld ("stack.399",V "Ti149.724.1225",4),
    Let (("Ti151.727.1227", Int),  Sub ("sp.400.1071",C 2 ),
    Let (("v1.728.1228", Int),  Ld ("stack.399",V "Ti151.727.1227",4),
    Let (("n.729.1229", Int),  IfLE ("v1.728.1228",V "v2.725.1226",
    Ans (Set (1)),
    Ans (Set (0))),
    Let (("Ti153.731.1230", Int),  Sub ("sp.400.1071",C 2 ),
    Let (("Tu12.732.1231", Unit),  St ("n.729.1229","stack.399",V "Ti153.731.1230",4),
    Let (("Ti155.734.1232", Int),  Sub ("sp.400.1071",C 1 ),
    Let (("sp.400.1294", Int),  Mov ("Ti155.734.1232"),
    Let (("Ti191.680.1421", Int),  Sub ("sp.400.1294",C 1 ),
    Let (("v.681.1422", Int),  Ld ("stack.399",V "Ti191.680.1421",4),
    Let (("sp2.683.1423", Int),  Sub ("sp.400.1294",C 1 ),
    Ans (IfEq ("v.681.1422",C 0 ,
    Let (("sp.400.1517", Int),  Mov ("sp2.683.1423"),
    Let (("Ti242.609.1606", Int),  Sub ("sp.400.1517",C 2 ),
    Let (("Ti244.611.1607", Int),  Sub ("Ti242.609.1606",C 1 ),
    Let (("v.612.1608", Int),  Ld ("stack.399",V "Ti244.611.1607",4),
    Let (("Tu24.613.1609", Unit),  St ("v.612.1608","stack.399",V "sp.400.1517",4),
    Let (("Ti246.615.1610", Int),  Add ("sp.400.1517",C 1 ),
    Let (("sp.400.1740", Int),  Mov ("Ti246.615.1610"),
    Let (("c.689.1872", Int),  Set (1),
    Let (("Tu15.690.1873", Unit),  St ("c.689.1872","stack.399",V "sp.400.1740",4),
    Let (("Ti184.692.1874", Int),  Add ("sp.400.1740",C 1 ),
    Let (("sp.400.1963", Int),  Mov ("Ti184.692.1874"),
    Let (("Ti85.799.2165", Int),  Sub ("sp.400.1963",C 1 ),
    Let (("v2.800.2166", Int),  Ld ("stack.399",V "Ti85.799.2165",4),
    Let (("Ti87.802.2167", Int),  Sub ("sp.400.1963",C 2 ),
    Let (("v1.803.2168", Int),  Ld ("stack.399",V "Ti87.802.2167",4),
    Let (("Ti89.805.2169", Int),  Sub ("sp.400.1963",C 2 ),
    Let (("Ti90.806.2170", Int),  Sub ("v1.803.2168",V "v2.800.2166"),
    Let (("Tu6.807.2171", Unit),  St ("Ti90.806.2170","stack.399",V "Ti89.805.2169",4),
    Let (("Ti92.809.2172", Int),  Sub ("sp.400.1963",C 1 ),
    Let (("sp.400.2186", Int),  Mov ("Ti92.809.2172"),
    Let (("Ti340.500.2201", Int),  Sub ("sp.400.2186",C 1 ),
    Let (("v.501.2202", Int),  Ld ("stack.399",V "Ti340.500.2201",4),
    Let (("Tu33.502.2203", Unit),  St ("v.501.2202","stack.399",V "sp.400.2186",4),
    Let (("Ti342.504.2204", Int),  Add ("sp.400.2186",C 1 ),
    Let (("sp.400.2409", Int),  Mov ("Ti342.504.2204"),
    Let (("Ti208.663.2524", Int),  Set (31),
    Let (("Tu21.664.2525", Unit),  St ("Ti208.663.2524","stack.399",V "sp.400.2409",4),
    Let (("Ti210.666.2526", Int),  Add ("sp.400.2409",C 1 ),
    Let (("Ti211.667.2527", Int),  Set (200),
    Let (("Tu20.668.2528", Unit),  St ("Ti211.667.2527","stack.399",V "Ti210.666.2526",4),
    Let (("sp2.670.2529", Int),  Add ("sp.400.2409",C 2 ),
    Ans (CallDir (L "tracetj0.844",["stack.399"; "sp2.670.2529"; ],[]))))))))))))))))))))))))))))))))),
    Let (("pc.402.1292", Int),  Set (16),
    Let (("bytecode.401.1293", Int),  CallDir (L "restore_min_caml_bp",[],[]),
    Let (("Ti195.686.1424", Int),  Add ("pc.402.1292",C 2 ),
    Ans (CallDir (L "guard_tracetj0.844",["stack.399"; "sp2.683.1423"; "bytecode.401.1293"; "Ti195.686.1424"; ],[])))))))))))))))))))))))))))))
  ;;

  let t_straight_trace2 =
    Let (("sp.400.1517", Int),  Mov ("sp2.683.1423"),
    Let (("Ti242.609.1606", Int),  Sub ("sp.400.1517",C 2 ),
    Let (("Ti244.611.1607", Int),  Sub ("Ti242.609.1606",C 1 ),
    Let (("v.612.1608", Int),  Ld ("stack.399",V "Ti244.611.1607",4),
    Let (("Tu24.613.1609", Unit),  St ("v.612.1608","stack.399",V "sp.400.1517",4),
    Let (("Ti246.615.1610", Int),  Add ("sp.400.1517",C 1 ),
    Let (("sp.400.1740", Int),  Mov ("Ti246.615.1610"),
    Let (("c.689.1872", Int),  Set (1),
    Let (("Tu15.690.1873", Unit),  St ("c.689.1872","stack.399",V "sp.400.1740",4),
    Let (("Ti184.692.1874", Int),  Add ("sp.400.1740",C 1 ),
    Let (("sp.400.1963", Int),  Mov ("Ti184.692.1874"),
    Let (("Ti85.799.2165", Int),  Sub ("sp.400.1963",C 1 ),
    Let (("v2.800.2166", Int),  Ld ("stack.399",V "Ti85.799.2165",4),
    Let (("Ti87.802.2167", Int),  Sub ("sp.400.1963",C 2 ),
    Let (("v1.803.2168", Int),  Ld ("stack.399",V "Ti87.802.2167",4),
    Let (("Ti89.805.2169", Int),  Sub ("sp.400.1963",C 2 ),
    Let (("Ti90.806.2170", Int),  Sub ("v1.803.2168",V "v2.800.2166"),
    Let (("Tu6.807.2171", Unit),  St ("Ti90.806.2170","stack.399",V "Ti89.805.2169",4),
    Let (("Ti92.809.2172", Int),  Sub ("sp.400.1963",C 1 ),
    Let (("sp.400.2186", Int),  Mov ("Ti92.809.2172"),
    Let (("Ti340.500.2201", Int),  Sub ("sp.400.2186",C 1 ),
    Let (("v.501.2202", Int),  Ld ("stack.399",V "Ti340.500.2201",4),
    Let (("Tu33.502.2203", Unit),  St ("v.501.2202","stack.399",V "sp.400.2186",4),
    Let (("Ti342.504.2204", Int),  Add ("sp.400.2186",C 1 ),
    Let (("sp.400.2409", Int),  Mov ("Ti342.504.2204"),
    Let (("Ti208.663.2524", Int),  Set (31),
    Let (("Tu21.664.2525", Unit),  St ("Ti208.663.2524","stack.399",V "sp.400.2409",4),
    Let (("Ti210.666.2526", Int),  Add ("sp.400.2409",C 1 ),
    Let (("Ti211.667.2527", Int),  Set (200),
    Let (("Tu20.668.2528", Unit),  St ("Ti211.667.2527","stack.399",V "Ti210.666.2526",4),
    Let (("sp2.670.2529", Int),  Add ("sp.400.2409",C 2 ),
         Ans (CallDir (L "tracetj0.844",["stack.399"; "sp2.670.2529"; ],[])))))))))))))))))))))))))))))))))
  ;;

  let%test "const_fold_mov test1" =
    pp "\n[TEST] cnost_fold_mov test1\n";
    let r1 = Const_fold.(const_fold M_string.empty_env t_straight_trace2 |> elim_dead_exp) in
    true
  ;;

  let%test "const_fold test1" =
    pp "[TEST] Applying const_fold\n";
    let r1 = Const_fold.(
        const_fold M_string.empty_env t_trace1
        |> elim_dead_exp
        |> const_fold_mov M_string.empty_env
        |> const_fold_if M_string.empty_env) in
    r1 |> print_t; print_newline ();
    true
  ;;

  let%test "const_fold test2 (applying const_fold_stld)" =
    pp "[TEST] Applying const_fold + const_fold_stld\n";
    let r1 = Const_fold.(
        const_fold M_string.empty_env t_trace1
        |> elim_dead_exp
        |> const_fold_mov M_string.empty_env
        |> const_fold_if M_string.empty_env
        |> const_fold_stld' (Array.make 20 None, 10) M.empty) in
    r1 |> print_t; print_newline ();
    true;;

end)
