open Asm
open Type
open Core
open OUnit
open MethodJit
open JitConfig

let t =
  Let (("Ti2.21", Int), Set (10),
       Let (("Ti3.22", Int), Set (0),
            Let (("bytecode.23", Array (Int)), CallDir (Id.L ("min_caml_create_array"), ["Ti2.21"; "Ti3.22"], []),
                 Let (("Ti4.24", Int), Set (9),
                      Let (("Ti5.25", Int), Set (1),
                           Let (("Tu1.26", Unit), St ("Ti5.25", "bytecode.23", V "Ti4.24", 4),
                                Let (("Ti6.27", Int), Set (0),
                                     Let (("Ti7.28", Int), Set (0),
                                          Let (("Ti8.29", Int), CallDir (Id.L ("interp.17"), ["bytecode.23"; "Ti6.27"; "Ti7.28"], []),
                                               Ans (CallDir (Id.L ("min_caml_print_int"), ["Ti8.29"], [])))))))))))

let fundef =
  { name = Id.L ("interp.17")
  ; args = ["code.18"; "pc.19"; "a.20"]
  ; fargs = []
  ; body =
      Let (("instr.30", Int), Ld ("code.18", V "pc.19", 4),
           Let (("Ti9.31", Int), Set (0),
                Ans (
                  IfEq ("instr.30", V "Ti9.31",
                        Let (("Ti10.34", Int), Set (100),
                             Ans (IfLE ("a.20", V "Ti10.34",
                                        Let (("Ti11.35", Int), Set (1),
                                             Let (("Ti12.36", Int), Add ("Ti11.35", V "pc.19"),
                                                  Let (("Ti13.37", Int), Set (2),
                                                       Let (("Ti14.38", Int), Add ("Ti13.37", V "a.20"),
                                                            Ans (CallDir (Id.L ("interp.17"), ["code.18"; "Ti12.36"; "Ti14.38"], [])))))),
                                        Ans (Add ("a.20", V "a.20"))))),
                        Let (("Ti15.32", Int), Add ("a.20", V "a.20"),
                             Let (("Ti16.33", Int), Set (2),
                                  Ans (Add ("Ti15.32", V "Ti16.33"))))))))
  ; ret = Int
  }


let p = Prog ([], [fundef], t)

let _ =
  let reg = Array.create 1000 (Red 0) in
  let mem = Array.create 1000 (Red 0) in
  reg.(19) <- Green (0);
  for i = 0 to 9 do
    reg.(23 + (i * 4)) <- Green (0)
  done;
  let res = method_jit p (fundef.body) reg mem dummy_jit_args in
  print_string (EmitVirtual.to_string_t res)
