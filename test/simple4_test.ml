open Mincaml
open Baccaml_jit
open Asm
open Core
open OUnit
open Test_util

module MJ = Method_jit

let _ =
  Sys.getcwd () |> print_endline

let Prog (_, fundefs, main) as prog =
  In_channel.create ("simple4.ml")
  |> Lexing.from_channel
  |> virtualize


let bytecode =
  [|1;
    1;
    0;
    4;
    5; 10;
    3; 9; 12;
    2; 0; 12;
    4|]


let _ = run_test_tt_main begin
    "simple4_test" >::: [
      "method_jit" >::
      begin fun () ->
        let fundef = List.hd_exn fundefs in
        Emit_virtual.to_string_fundef fundef |> print_endline;
        let method_jit_args = Method_jit_args ({
            method_name = "min_caml_test_trace";
            reds = ["bytecode.79"; "a.82"];
            method_start = 0;
            method_end = 3;
            pc_place = 1;
            backedge_pcs = []
          }) in
        let { body } = fundef in
        let reg = Array.create 100000 (Red (0)) in
        let mem = Array.create 100000 (Red (0)) in
        reg.(80) <- Green (0);
        reg.(81) <- Green (6);
        reg.(82) <- Red (0);
        for i = 0 to (Array.length bytecode - 1) do
          let n = i * 4 in
          if n = 20 then mem.(n) <- Red (bytecode.(i))
          else mem.(n) <- Green (bytecode.(i))
        done;
        let res = MJ.exec prog body reg mem method_jit_args in
        (match res with
         | Tracing_success res' | Method_success res' ->
           (Emit_virtual.to_string_fundef res') |> print_endline);
        Jit_emit.emit_trace
          res
          "simple4_mj"
          "interp.79"
      end
    ]
  end
