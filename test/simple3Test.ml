open Asm
open Core
open OUnit
open TracingJit
open MethodJit
open JitConfig
open MincamlUtil
open TestUtil

let Prog (_, fundefs, main) as prog =
  In_channel.create (dir ^ "simple3.ml")
  |> Lexing.from_channel
  |> virtualize

let bytecode = [|22; 2; 22; 2; 0; 11; 2; 10; 0; 4; 5; 30|]

let _ = run_test_tt_main begin
    "simple3_test" >::: [
      "method_jit" >::
      begin fun () ->
        let fundef = List.hd_exn fundefs in
        let method_jit_args = {
          method_name = "test_method.1000";
          reds = ["stack.107"; "sp.108"];
          method_start = 0;
          method_end = 6;
          pc_place = 1
        } in
        let { body } = fundef in
        let reg = Array.create 10000 (Red 0) in
        let mem = Array.create 10000 (Red 0) in
        reg.(105) <- Green (0);
        reg.(106) <- Green (7);
        reg.(107) <- Red (100);
        reg.(108) <- Red (0);
        for i = 0 to (Array.length bytecode - 1) do
          mem.(i * 4) <- Green (bytecode.(i))
        done;
        let res = method_jit prog body reg mem method_jit_args in
        print_string (EmitVirtual.to_string_t res);
        print_newline ();
        ()
      end;
      "tracing_jit" >::
      begin fun () ->
        let { body } = List.hd_exn fundefs in
        let tracing_jit_args = {
          trace_name = "test_trace.1000";
          reds = ["stack.107"; "sp.108"];
          greens = [];
          loop_header = 7;
          loop_pc_place = 1;
        } in
        let reg = Array.create 10000 (Red 0) in
        let mem = Array.create 10000 (Red 0) in
        reg.(105) <- Green (0);
        reg.(106) <- Green (7);
        reg.(107) <- Red (100);
        reg.(108) <- Red (0);
        for i = 0 to (Array.length bytecode - 1) do
          mem.(i * 4) <- Green (bytecode.(i))
        done;
        let res = TracingJit.tracing_jit prog body reg mem tracing_jit_args in
        print_string (EmitVirtual.to_string_t res);
      end
    ]
  end
