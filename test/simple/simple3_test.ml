open MinCaml
open BacCaml
open Util
open Asm
open Core
open OUnit
open Test_util

module TJ = Tracing_jit
module MJ = Method_jit

let Prog (_, fundefs, main) as prog =
  In_channel.create ("simple3.ml")
  |> Lexing.from_channel |> virtualize

let bytecode = [|22; 2; 22; 2; 0; 11; 12; 9; 13; 10; 0; 4; 5; 11|]

let _ = run_test_tt_main begin
    "simple3_test" >::: [
      "method_jit" >::
      begin fun () ->
        let fundef = List.hd_exn fundefs in
        let method_jit_args = Method_jit_args ({
            method_name = "min_caml_test_trace";
            reds = [];
            method_start = 0;
            method_end = 6;
            pc_place = 1
          }) in
        let { body } = fundef in
        let reg = Array.create 10000 (Red 0) in
        let mem = Array.create 10000 (Red 0) in
        reg.(126) <- Green (0);
        reg.(127) <- Green (6);
        reg.(128) <- Green (100);
        reg.(129) <- Green (1);
        for i = 0 to (Array.length bytecode - 1) do
          let n = i * 4 in
          if n = 44 then
            mem.(n) <- Red (bytecode.(i))
          else if n = 48 then
            mem.(n) <- Red (bytecode.(i))
          else
            mem.(i * 4) <- Green (bytecode.(i))
        done;
        let trace = MJ.exec prog body reg mem method_jit_args in
        (match trace with
         | Tracing_success res' | Method_success res' ->
           Emit_virtual.to_string_fundef res' |> print_endline);
        Jit_emit.emit_trace
          trace
          "simple3_mj"
          "interp.126"
      end;
      "tracing_jit" >::
      begin fun () ->
        let { body } = List.hd_exn fundefs in
        let tracing_jit_args = Tracing_jit_args ({
            trace_name = "min_caml_test_trace";
            reds = [];
            greens = [];
            loop_header = 6;
            loop_pc_place = 1;
          }) in
        let reg = Array.create 100000 (Red 0) in
        let mem = Array.create 100000 (Red 0) in
        reg.(126) <- Green (0);
        reg.(127) <- Green (6);
        reg.(128) <- Green (100);
        reg.(129) <- Green (1);
        for i = 0 to (Array.length bytecode - 1) do
          let n = i * 4 in
          if n = 44 then
            mem.(n) <- Red (bytecode.(i))
          else if n = 48 then
            mem.(n) <- Red (bytecode.(i))
          else
            mem.(i * 4) <- Green (bytecode.(i))
        done;
        let trace = TJ.exec prog body reg mem tracing_jit_args in
        (match trace with
         | Tracing_success res' | Method_success res' ->
           Emit_virtual.to_string_fundef res' |> print_endline)
      end
    ]
  end
