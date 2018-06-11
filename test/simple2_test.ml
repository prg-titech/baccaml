open Mincaml
open Baccaml_jit
open Asm
open Core
open Type
open Core
open OUnit
open Test_util

module TJ = Tracing_jit
module MJ = Method_jit
module JE = Jit_emit

let Prog (_, fundefs, main) as prog =
  In_channel.create ("simple2.ml")
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
        let method_jit_args = Method_jit_args (
            { method_name = "min_caml_test_trace";
              reds = ["code.70"; "a.72"];
              method_start = 0;
              method_end = 12;
              pc_place = 1
            }) in
        let reg = Array.create 10000 (Red (0)) in
        let mem = Array.create 10000 (Red (0)) in
        for i = 0 to (Array.length bytecode - 1) do
          mem.(0 + i * 4) <- Green (bytecode.(i))
        done;
        reg.(70) <- Green (0);
        reg.(71) <- Green (0);
        let res = MJ.exec prog body reg mem method_jit_args in
        (match res with
         | Tracing_success res' | Method_success res' ->
           Emit_virtual.to_string_fundef res' |> print_endline);
        Jit_emit.emit_trace
          res
          "simple2_mj"
          "interp.69"
      end;
      "tracing_jit" >:: begin fun () ->
        let tracing_jit_args = Tracing_jit_args (
            { trace_name = "min_caml_test_trace";
              reds = ["code.70"; "a.72"];
              greens = [];
              loop_header = 0;
              loop_pc_place = 1
            }) in
        let reg = Array.create 100000 (Red (0)) in
        let mem = Array.create 100000 (Red (0)) in
        reg.(70) <- Green (0);
        reg.(71) <- Green (0);
        for i = 0 to (Array.length bytecode - 1) do
          mem.(0 + i * 4) <- Green (bytecode.(i))
        done;
        let res = TJ.exec prog body reg mem tracing_jit_args in
        (match res with
         | Tracing_success res' | Method_success res' ->
           Emit_virtual.to_string_fundef res' |> print_endline);
      end
    ]
  end
