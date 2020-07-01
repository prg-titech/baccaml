open MinCaml
open Asm
open Opt_defuse
open Opt_lib

let%test_module "constfold test" =
  (module struct
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
    [@@ocamlformat "disable"]

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
    [@@ocamlformat "disable"]

    let t_straight_trace3 =
      Let (("Ti242.609", Int),  Sub ("sp.400",C 4 ),
      Let (("Ti244.611", Int),  Sub ("Ti242.609",C 1 ),
      Let (("v.612", Int),  Ld ("stack.399",V "Ti244.611",4),
      Let (("Tu24.613", Unit),  St ("v.612","stack.399",V "sp.400",4),
      Let (("Ti246.615", Int),  Add ("sp.400",C 1 ),
      Let (("sp.400.861", Int),  Mov ("Ti246.615"),
      Let (("Ti242.609.950", Int),  Sub ("sp.400.861",C 1 ),
      Let (("Ti244.611.951", Int),  Sub ("Ti242.609.950",C 1 ),
      Let (("v.612.952", Int),  Ld ("stack.399",V "Ti244.611.951",4),
      Let (("Tu24.613.953", Unit),  St ("v.612.952","stack.399",V "sp.400.861",4),
      Let (("Ti246.615.954", Int),  Add ("sp.400.861",C 1 ),
      Let (("sp.400.1084", Int),  Mov ("Ti246.615.954"),
      Let (("Ti73.813.1295", Int),  Sub ("sp.400.1084",C 1 ),
      Let (("v2.814.1296", Int),  Ld ("stack.399",V "Ti73.813.1295",4),
      Let (("Ti75.816.1297", Int),  Sub ("sp.400.1084",C 2 ),
      Let (("v1.817.1298", Int),  Ld ("stack.399",V "Ti75.816.1297",4),
      Let (("Ti77.819.1299", Int),  Sub ("sp.400.1084",C 2 ),
      Let (("Ti78.820.1300", Int),  Add ("v1.817.1298",V "v2.814.1296"),
      Let (("Tu5.821.1301", Unit),  St ("Ti78.820.1300","stack.399",V "Ti77.819.1299",4),
      Let (("Ti80.823.1302", Int),  Sub ("sp.400.1084",C 1 ),
      Let (("sp.400.1307", Int),  Mov ("Ti80.823.1302"),
      Let (("Ti251.597.1388", Int),  Sub ("sp.400.1307",C 1 ),
      Let (("v.598.1389", Int),  Ld ("stack.399",V "Ti251.597.1388",4),
      Let (("Ti255.600.1390", Int),  Sub ("sp.400.1307",C 2 ),
      Let (("Tu26.601.1391", Unit),  St ("v.598.1389","stack.399",V "Ti255.600.1390",4),
      Let (("Ti257.603.1392", Int),  Sub ("sp.400.1307",C 1 ),
      Let (("sp.400.1530", Int),  Mov ("Ti257.603.1392"),
      Let (("Ti251.597.1611", Int),  Sub ("sp.400.1530",C 1 ),
      Let (("v.598.1612", Int),  Ld ("stack.399",V "Ti251.597.1611",4),
      Let (("Ti255.600.1613", Int),  Sub ("sp.400.1530",C 2 ),
      Let (("Tu26.601.1614", Unit),  St ("v.598.1612","stack.399",V "Ti255.600.1613",4),
      Let (("Ti257.603.1615", Int),  Sub ("sp.400.1530",C 1 ),
      Let (("sp.400.1753", Int),  Mov ("Ti257.603.1615"),
      Let (("Ti223.621.1849", Int),  Sub ("sp.400.1753",C 1 ),
      Let (("v.622.1850", Int),  Ld ("stack.399",V "Ti223.621.1849",4),
      Let (("Ti225.624.1851", Int),  Sub ("sp.400.1753",C 2 ),
      Let (("mode.625.1852", Int),  Ld ("stack.399",V "Ti225.624.1851",4),
      Let (("Ti227.627.1853", Int),  Sub ("sp.400.1753",C 3 ),
      Let (("addr.628.1854", Int),  Ld ("stack.399",V "Ti227.627.1853",4),
      Ans (IfEq ("mode.625.1852",C 200 ,
      Let (("Ti231.633.1857", Int),  Sub ("sp.400.1753",C 1 ),
      Let (("Ti233.635.1858", Int),  Sub ("Ti231.633.1857",C 3 ),
      Let (("Tu23.636.1859", Unit),  St ("v.622.1850","stack.399",V "Ti233.635.1858",4),
      Let (("Ti234.637.1860", Int),  Sub ("sp.400.1753",C 1 ),
      Let (("sp2.639.1861", Int),  Sub ("Ti234.637.1860",C 2 ),
      Ans (IfGE ("addr.628.1854",C 38 ,
      Let (("bytecode.401.1752", Int),  CallDir (L "restore_min_caml_bp",[],[]),
      Ans (CallDir (L "guard_tracetj1.857",["stack.399"; "sp2.639.1861"; "bytecode.401.1752"; "addr.628.1854"; ],[]))),
      Let (("sp.400.1976", Int),  Mov ("sp2.639.1861"),
      Let (("pc.402.1974", Int),  Mov ("addr.628.1854"),
      Ans (CallDir (L "tracetj1.857",["stack.399"; "sp.400.1976"; ],[]))))))))))),
      Ans (Mov ("v.622.1850")))))))))))))))))))))))))))))))))))))))))))
    [@@ocamlformat "disable"]

    let%test "const_fold test2 (applying const_fold_stld)" =
      print_newline ();
      print_string "\027[32m";
      print_string "[TEST] Applying const_fold + const_fold_stld\n";
      print_string "\027[0m";
      let r1 =
        Opt_const_fold.(
          const_fold_exp t_trace1
          |> const_fold_if
          |> const_fold_mov
          |> Opt_guard.move_into_guard
          |> elim_dead_exp)
        |> Opt_mem.(remove_rw M.empty M'.empty)
      in
      r1 |> print_t;
      print_newline ();
      flush_all ();
      true
    ;;

    let%test "mem_opt test" =
      print_newline ();
      print_string "\027[32m";
      print_string "[TEST] Applying const_fold + mem_opt\n";
      print_string "\027[0m";
      let r1 =
        Opt_const_fold.(
          const_fold_exp t_trace1
          |> const_fold_mov
          |> const_fold_if
          |> Opt_guard.move_into_guard
          |> const_fold_id
          |> elim_dead_exp)
      in
      let r2 =
        Opt_mem.const_fold_rw r1
        |> Opt_const_fold.elim_dead_exp
        |> Opt_const_fold.const_fold_mov
      in
      print_t r2;
      print_newline ();
      true
    ;;

    let%test "integration test" =
      print_newline ();
      print_string "\027[32m";
      print_string "[TEST] integration test\n";
      print_string "\027[0m";
      let r1 = Opt_defuse.exec t_trace1 |> Simm.t in
      print_t r1;
      print_newline ();
      let r2 = Opt_defuse.exec t_straight_trace3 in
      print_t r2;
      print_newline ();
      true
    ;;
  end)
;;
