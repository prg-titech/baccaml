open Core
open OUnit

open MinCaml
open Asm
open Util

open Bc_jit
open Jit_config

module MJ = Method_jit

let _ =
  Sys.getcwd () |> print_endline

let Prog (_, fundefs, main) as prog =
  In_channel.create ("simple5.ml")
  |> Lexing.from_channel
  |> virtualize


let bytecode =
  [|1;
    1;
    0;
    10;
    5; 100;
    3; 9; 14;
    2; 0; 12;
    4; 6;
    10|]


let _ = run_test_tt_main begin
    "simple5_test" >::: [
      "method_jit" >::
      begin fun () ->
        let fundef = List.hd_exn fundefs in
        let method_jit_args = Method_jit_args (
            { method_name = "min_caml_test_trace";
              reds = ["bytecode.89"; "a.91"];
              method_start = 0;
              method_end = 3;
              pc_place = 1;
              loop_headers = [4];
              backedge_pcs = [6]
            }) in
        let { body } = fundef in
        let reg = Array.make 100000 (Red (0)) in
        let mem = Array.make 100000 (Red (0)) in
        reg.(89) <- Green (0);
        reg.(90) <- Green (6);
        reg.(91) <- Red (0);
        for i = 0 to (Array.length bytecode - 1) do
          let n = i * 4 in
          if n = 20 then mem.(n) <- Red (bytecode.(i))
          else mem.(n) <- Green (bytecode.(i))
        done;
        let res = match MJ.exec prog body reg mem method_jit_args with
          | Tracing_success res'
          | Method_success res' -> res' in
        Jit_emit.emit_trace
          `Meta_method
          res
          "simple5_mj"
          "interp.88"
      end
    ]
  end
