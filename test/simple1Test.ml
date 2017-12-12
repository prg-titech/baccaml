open OUnit
open Asm
open Jit
open JitUtil
open MincamlUtil

let dir = "min-interp/"

let _ = run_test_tt_main begin
    "tracing jit test" >::: [
      "test 1" >::
      begin fun () ->
        let prog =
          open_in (dir ^ "simple1.ml")
          |> Lexing.from_channel
          |> virtualize
        in
        let Prog (_, [fundef], main) = prog in
        let instr = fundef.body in
        let reg = Array.make 1000 (Red 0) in
        let mem = Array.make 1000 (Red 0) in
        mem.(0 * 4) <- Green (1);
        mem.(1 * 4) <- Green (2);
        mem.(2 * 4) <- Green (0);
        mem.(3 * 4) <- Green (4);
        reg.(40) <- Green (0);
        reg.(41) <- Green (0);
        reg.(42) <- Red (100);
        let jit_args =
          { trace_name = "test_trace.1000";
            reds = ["a.42"];
            loop_header = 0;
            loop_pc = 41; }
        in
        let res = exec_jitcompile prog instr reg mem jit_args in
        let prog' = Prog ([], fundef :: res :: [], main) in
        let reg' = Array.make 10000 0 in
        let mem' = Array.make 10000 0 in
        JitUtil.is_jit := true;
        reg'.(42) <- 100;
        mem'.(12) <- 4;
        let _ = Interp.interp
            (Interp.to_prog_with_label prog')
            main
            reg'
            mem'
            jit_args
        in
        ()
      end
    ]
  end
