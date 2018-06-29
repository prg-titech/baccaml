open OUnit
open Mincaml
open Asm
open Util
open Baccaml_jit
open Jit_config
open Jit_util

module MJ = Method_jit

let print_list f lst =
  let rec loop f = function
    | [] -> ()
    | hd :: tl -> f hd; print_string "; "; loop f tl
  in
  print_string "["; loop f lst; print_string "]"

let _ = run_test_tt_main begin
    "mj_loop_test" >::: [
      "test1" >::
      begin fun () ->
        Logger.log_level := Logger.Debug;
        let t =
          Let ((Id.gentmp Type.Unit, Type.Unit), CallDir (Id.L "min_caml_loop_start", [], []),
                     Let (("x.10", Type.Int), Add ("a.9", V ("b.8")),
                          Let (("ans.11", Type.Int), Add ("x.10", V ("x.10")),
                               Let ((Id.gentmp Type.Unit, Type.Unit), CallDir (Id.L "min_caml_loop_end", [], []),
                                    Let (("y.12", Type.Int), Sub ("ans.11", C (100)),
                                         Ans (Mov ("y.12"))))))) in
        let p = Prog ([], [], Ans (Nop)) in
        let reg = Array.make 100 (Red (0)) in
        let mem = Array.make 100 (Red (0)) in
        let res = Method_jit_loop.find_loop_start_pc p reg mem t in
        print_list print_int res
      end;
      "test2" >::
      begin fun () ->
        Logger.log_level := Logger.Debug;
        (* preprocesses *)
        let bytecode = [|1; 1; 7; 1; 10; 4; 2; 13; 6; 0; 5; 3; 5; 7 |] in
        let p =
          open_in ((Sys.getcwd ()) ^ "/test/jit_loop.ml")
          |> Lexing.from_channel
          |> Mutil.virtualize
          |> Simm.f
        in
        let Prog (_, fundefs, main) = p in
        let fundef = List.hd fundefs in
        let { body; } = fundef in
        let reg = Array.make 10000 (Red (-1)) in
        let mem = Array.make 10000 (Red (-1)) in
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
        (* execute preprocessor *)
        let fundefs', interp_body, jit_args' =
          Method_jit.prep p body reg mem method_jit_args
        in
        
        let fundef' = List.hd fundefs' in
        let redtbl = Hashtbl.create 100 in
        let greentbl = Hashtbl.create 100 in
        Hashtbl.add greentbl "bytecode" 0;
        Hashtbl.add greentbl "pc" 3;
        Hashtbl.add redtbl "a" 100;
        Colorizer.colorize_reg redtbl greentbl reg fundef' interp_body;
        Colorizer.colorize_pgm bytecode 0 mem;

        (* execute function jit *)
        let res = match Method_jit.exec p body reg mem method_jit_args with
          | Method_success fundef | Tracing_success fundef -> fundef
        in
        print_endline "[RESULT]" |> fun () -> Asm.show_fundef res |> print_endline;

        (* extract loop function *)
        let loop = Method_jit.find_loop "test_loop_fun" res.body in
        print_endline "[LOOP FUNCTION]" |> fun () -> Asm.show_fundef loop |> print_endline;
        
        (* extract non loop function *)
        let nonloop = Method_jit.find_nonloop "test_loop_fun" res.body in
        print_endline "[NONLOOP FUNCTION]" |> fun () -> Emit_virtual.to_string_t nonloop |> print_endline;
        ()
      end;
    ]
  end
