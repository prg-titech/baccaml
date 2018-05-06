open Mincaml
open Baccaml_jit
open Asm
open Core
open OUnit
open Test_util

module TJ = Tracing_jit

module JE = Jit_emit

let jit_args =
  { trace_name = "min_caml_test_trace"
  ; reds = ["a.44"]
  ; greens = []
  ; loop_header = 0
  ; loop_pc_place = 1 }

let _ = run_test_tt_main begin
    "tracing_jit_test" >::: [
      "simple1_test" >:: begin fun () ->
        let prog =
          In_channel.create ("simple1.ml")
          |> Lexing.from_channel
          |> virtualize
          |> Simm.f
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
        reg.(42) <- Green (0);
        reg.(43) <- Green (0);
        reg.(44) <- Red (100);
        let res = TJ.exec prog body reg mem jit_args in
        print_string (Emit_virtual.to_string_fundef res);
        JE.emit_trace' ~fundef:res ~fname:"simple1_tj" ~inameo:"interpret.39" ~inamen:"interpret.40"
      end
    ]
  end
