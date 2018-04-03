open Asm
open Core
open OUnit
open Tracing_jit
open Jit_config
open Mincaml_util
open Test_util

let jit_args =
  { trace_name = "min_caml_test_trace"
  ; reds = ["a.42"]
  ; greens = []
  ; loop_header = 0
  ; loop_pc_place = 1 }

let _ = Logger.log_level := Logger.Debug

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
        print_string (Emit_virtual.to_string_fundef res);
        Jit_emit.emit_asm res "simple1_tj";
        Jit_emit.emit_trace res "simple1_tj" "interpret.40"
      end
    ]
  end
