open Asm
open Core
open OUnit
open TracingJit
open JitConfig
open MincamlUtil
open TestUtil

let jit_args =
  { trace_name = "test_trace.1000"
  ; reds = ["a.42"]
  ; greens = []
  ; loop_header = 0
  ; loop_pc_place = 1 }

let _ = run_test_tt_main begin
    "tracing_jit_test" >::: [
      "simple1_test" >:: begin fun () ->
        let prog =
          In_channel.create (dir ^ "simple1.ml")
          |> Lexing.from_channel
          |> virtualize
        in
        let Prog (_, fundefs, main) = prog in
        let fundef = List.hd_exn fundefs in
        let { body; } = fundef in
        let reg = Array.create 1000 (Red 0) in
        let mem = Array.create 1000 (Red 0) in
        let reg' = Array.create 10000 0 in
        let mem' = Array.create 10000 0 in
        mem.(0 * 4) <- Green (1);
        mem.(1 * 4) <- Green (2);
        mem.(2 * 4) <- Green (0);
        mem.(3 * 4) <- Green (4);
        reg.(40) <- Green (0);
        reg.(41) <- Green (0);
        reg.(42) <- Red (100);
        let res = exec_tracing_jit prog body reg mem jit_args in
        let prog' = Prog ([], fundef :: res :: [], main) in
        setup reg reg';
        setup mem mem';
        Logger.log_level := Logger.Debug;
        enable_jit := true;
        print_string (EmitVirtual.to_string_fundef res);
      end
    ]
  end
