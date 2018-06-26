open Core
open OUnit
open Mincaml
open Asm
open Util
open Baccaml_jit
open Jit_config

module MJ = Method_jit

let bytecode = [|1; 1; 7; 4; 6; 0; 5; 2; 3; 7|]

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
      "test2" >::
      begin fun () ->
        Logger.log_level := Logger.Debug;
        let p =
          In_channel.create ((Sys.getcwd ()) ^ "/test/jit_loop.ml")
          |> Lexing.from_channel
          |> Mutil.virtualize
          |> Simm.f
        in
        let Prog (_, fundefs, main) = p in
        let fundef = List.hd_exn fundefs in
        let { body; } = fundef in
        let reg = Array.create 100000 (Red 0) in
        let mem = Array.create 100000 (Red 0) in
        let method_jit_args =
          Method_jit_args (
            { method_name = "min_caml_test_trace";
              reds = ["bytecode.72"; "a.74"];
              method_start = 0;
              method_end = 3;
              pc_place = 1;
              loop_headers = [0];
              backedge_pcs = [0]
            })
        in
        reg.(77) <- Green (0);
        reg.(78) <- Green (3);
        reg.(79) <- Red (100);
        for i = 0 to (Array.length bytecode - 1) do
          mem.(77 + 4 * i) <- Green (bytecode.(i))
        done;
        Trim.flg := true;
        let t = match body |> Trim.trim_jmp with
          | Ans (IfEq (_, _, Ans (CallDir _), body')) -> body'
          | _ -> body
        in
        let fundef' = { name = fundef.name; args = fundef.args; fargs = fundef.fargs; body = t; ret = fundef.ret } in
        Emit_virtual.to_string_fundef fundef' |> print_endline;
        let p' = Prog ([], [fundef'], main) in
        let f = MJ.find_loop p' t reg mem method_jit_args in
        Asm.show f |> print_endline;
        ()
      end
    ]
  end
