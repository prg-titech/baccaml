open Asm
open Core
open OUnit
open TracingJit
open MethodJit
open JitConfig
open MincamlUtil
open TestUtil

let _ = run_test_tt_main begin
    "method_jit_test" >::: [
      "simple3_test" >::
      begin fun () ->
        let prog =
          In_channel.create (dir ^ "simple3.ml")
          |> Lexing.from_channel
          |> virtualize
        in
        let Prog (_, fundefs, main) = prog in
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
        reg.(108) <- Red (1);
        let bytecode = [|22; 2; 22; 2; 0; 11; 2; 10; 0; 4; 5; 30|] in
        for i = 0 to (Array.length bytecode - 1) do
          mem.(i * 4) <- Green (bytecode.(i))
        done;
        mem.(100) <- Green (4); mem.(104) <- Green (5);
        let res = method_jit prog body reg mem method_jit_args in
        print_string (EmitVirtual.to_string_t res);
        ()
      end
    ]
  end
