open Core
open OUnit
open Mincaml
open Baccaml_jit
open Jit_config
open Asm

let _ = run_test_tt_main begin
    "mj_loop_test" >::: [
      "test1" >::
      begin fun () ->
        let t = Let ((Id.gentmp Type.Unit, Type.Unit), CallDir (Id.L "min_caml_loop_start", [], []),
                     Let (("x.10", Type.Int), Add ("a.9", V ("b.8")),
                          Let (("ans.11", Type.Int), Add ("x.10", V ("x.10")),
                               Let ((Id.gentmp Type.Unit, Type.Unit), CallDir (Id.L "min_caml_loop_end", [], []),
                                    Let (("y.12", Type.Int), Sub ("ans.11", C (100)),
                                         Ans (Mov ("y.12"))))))) in
        let p = Prog ([], [], Ans (Nop)) in
        let reg = Array.create 100 (Red (0)) in
        let mem = Array.create 100 (Red (0)) in
        let mjargs =  Method_jit_args (
            { method_name = "min_caml_test_trace";
              reds = ["bytecode.89"; "a.91"];
              method_start = 0;
              method_end = 3;
              pc_place = 1;
              loop_headers = [4];
              backedge_pcs = [6]
            })
        in
        let res = Method_jit.find_loop p t reg mem mjargs in
        print_endline (Asm.show res)
      end;
    ]
  end
