open Asm
open Core
open OUnit
open JitConfig
open TracingJit
open MincamlUtil

let string_of_value = function
    Green n -> "Green (" ^ (string_of_int n) ^ ")"
  | Red n -> "Red (" ^ (string_of_int n) ^ ")"

let rec string_of_value_list = function
    [] -> "|]"
  | x :: y ->
    "[| " ^ " " ^ (string_of_value x) ^ (string_of_value_list y)

let prog =
  In_channel.create (TestUtil.dir ^ "pypyfig3.ml")
  |> Lexing.from_channel
  |> virtualize

let _ = run_test_tt_main begin
    "tracing_jit_test" >::: [
      "convert_test" >:: begin
        fun () ->
          let reds = ["a"; "regs"] in
          let greens = ["bytecode"; "pc"] in
          let reg = Array.create 1000 0 in
          reg.(107) <- 0;
          reg.(108) <- 4;
          reg.(109) <- 100;
          reg.(110) <- 400;
          let jit_regs = JitInterop.convert prog reg reds greens in
          assert_equal
            ~printer:string_of_value
            (jit_regs.(109))
            (Red 100);
          assert_equal
            ~printer:string_of_value
            (jit_regs.(110))
            (Red 400);
          assert_equal
            ~printer:string_of_value
            (jit_regs.(107))
            (Green 0);
          assert_equal
            ~printer:string_of_value
            (jit_regs.(108))
            (Green 4)
      end;
      "pypyfig3_test" >:: begin
        fun () ->
          JitConfig.enable_jit := true;
          Logger.log_level := Logger.Debug;
          let Prog (_, fundefs, main) = prog in
          let fundef = match fundefs with
            | [fundef] -> fundef
            | _ -> failwith "Error."
          in
          let { body } = fundef in
          let reds = ["a"; "regs"] in
          let greens = ["bytecode"; "pc"] in
          let reg = Array.create 10000 0 in
          let mem = Array.create 10000 0 in
          reg.(107) <- 0;
          reg.(108) <- 4;
          reg.(109) <- 100;
          reg.(110) <- 400;
          let jit_reg = JitInterop.convert prog reg reds greens in
          let jit_mem = Array.create 1000 (Green 0) in
          jit_mem.(0 * 4) <- Green (1);
          jit_mem.(1 * 4) <- Green (0);
          jit_mem.(2 * 4) <- Green (1);
          jit_mem.(3 * 4) <- Green (2);
          jit_mem.(4 * 4) <- Green (2);
          jit_mem.(5 * 4) <- Green (0);
          jit_mem.(6 * 4) <- Green (4);
          jit_mem.(7 * 4) <- Green (1);
          jit_mem.(8 * 4) <- Green (0);
          jit_mem.(9 * 4) <- Green (2);
          jit_mem.(10 * 4) <- Green (2);
          jit_mem.(11 * 4) <- Green (3);
          jit_mem.(12 * 4) <- Green (1);
          jit_mem.(13 * 4) <- Green (1);
          jit_mem.(14 * 4) <- Green (2);
          jit_mem.(15 * 4) <- Green (2);
          jit_mem.(16 * 4) <- Green (0);
          jit_mem.(17 * 4) <- Green (0);
          jit_mem.(18 * 4) <- Green (4);
          jit_mem.(19 * 4) <- Green (2);
          jit_mem.(20 * 4) <- Green (2);
          jit_mem.(21 * 4) <- Green (5);
          jit_mem.(400) <- Red (100);
          jit_mem.(400 + 16) <- Red (100);
          jit_mem.(400 + 32) <- Red (100);
          let jit_args =
            { trace_name = "test_trace.1000"
            ; reds = ["a.109"; "regs.110"]
            ; greens = []
            ; loop_header = 4
            ; loop_pc_place = 1 }
          in
          let trace = exec_tracing_jit prog body jit_reg jit_mem jit_args in
          let prog' = Prog ([], fundef :: trace :: [], main) in
          print_string (EmitVirtual.to_string_fundef trace);
          TestUtil.setup jit_mem mem;
          (*ignore (Interp.interp (InterpConfig.to_prog_with_label prog') main jit_reg' jit_mem' jit_args);*)
          (*prog' |> Simm.f |> Jit_RegAlloc.f |> Emit.f (Out_channel.create ("test/pypyfig3.s"));*)
          ()
      end
    ]
  end
