open Asm
open Core
open Type
open Core
open OUnit
open Method_jit
open Tracing_jit
open Jit_config
open Mincaml_util
open Test_util

module JE = Jit_emit

let Prog (_, fundefs, main) as prog =
  In_channel.create (dir ^ "simple2.ml")
  |> Lexing.from_channel
  |> virtualize

let fundef = match List.hd fundefs with
    Some v -> v
  | None -> failwith "List.hd is failed."

let { body; } = fundef

let trace_main = Ans(Nop)

let bytecode = [|0; 11; 4; 8; 0; 0; 10; 12; 1; 1; 10; 12; 20|]

let _ = run_test_tt_main begin
    "simple2_test" >::: [
      "method_jit" >:: begin fun () ->
        let method_jit_args = {
          method_name = "min_caml_test_trace";
          reds = ["a.69"];
          method_start = 0;
          method_end = 12;
          pc_place = 1
        } in
        let reg = Array.create 10000 (Red (0)) in
        let mem = Array.create 10000 (Red (0)) in
        for i = 0 to (Array.length bytecode - 1) do
          mem.(0 + i * 4) <- Green (bytecode.(i))
        done;
        reg.(67) <- Green (0);
        reg.(68) <- Green (0);
        let res = exec_method_jit prog body reg mem method_jit_args in
        Out_channel.print_endline (Emit_virtual.to_string_fundef res);
        Jit_emit.emit_trace'
          ~fundef:res
          ~fname:"simple2_mj"
          ~inameo:"interp.66"
          ~inamen:"interp.67"
      end;
      "tracing_jit" >:: begin fun () ->
        let tracing_jit_args = {
          trace_name = "min_caml_test_trace";
          reds = ["a.69"];
          greens = [];
          loop_header = 0;
          loop_pc_place = 1
        } in
        let reg = Array.create 100000 (Red (0)) in
        let mem = Array.create 100000 (Red (0)) in
        reg.(67) <- Green (0);
        reg.(68) <- Green (0);
        for i = 0 to (Array.length bytecode - 1) do
          mem.(0 + i * 4) <- Green (bytecode.(i))
        done;
        let res = exec_tracing_jit prog body reg mem tracing_jit_args in
        Out_channel.print_endline (Emit_virtual.to_string_fundef res);
        (* Jit_compiler.compile (Prog ([], res :: [], trace_main)) "test/simple2_tj.s"; *)
      end
    ]
  end
