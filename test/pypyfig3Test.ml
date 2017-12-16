open OUnit
open Asm
open Jit
open JitUtil
open Util
open MincamlUtil

let dir = "min-interp/"

let setup aa bb =
  List.iter (fun (a, i) -> bb.(i) <- value_of a)
    (List.zip (Array.to_list aa) (List.range 0 (Array.length aa - 1)))

let _ = run_test_tt_main begin
    "tracing_jit_test" >::: [
      "pypyfig3_test" >::
      begin fun () ->
        let prog =
          open_in (dir ^ "pypyfig3.ml")
          |> Lexing.from_channel
          |> virtualize
        in
        let Prog (_, fundefs, main) = prog in
        let fundef = match fundefs with
          | [fundef] -> fundef
          | _ -> failwith "Error."
        in
        let instr = fundef.body in
        let reg = Array.make 1000 (Red 0) in
        let mem = Array.make 1000 (Red 0) in
        reg.(107) <- Green (0);
        reg.(108) <- Green (4);
        reg.(109) <- Red (100);
        reg.(110) <- Red (100 * 4);
        mem.(0 * 4) <- Green (1);
        mem.(1 * 4) <- Green (0);
        mem.(2 * 4) <- Green (1);
        mem.(3 * 4) <- Green (2);
        mem.(4 * 4) <- Green (2);
        mem.(5 * 4) <- Green (0);
        mem.(6 * 4) <- Green (4);
        mem.(7 * 4) <- Green (1);
        mem.(8 * 4) <- Green (0);
        mem.(9 * 4) <- Green (2);
        mem.(10 * 4) <- Green (2);
        mem.(11 * 4) <- Green (3);
        mem.(12 * 4) <- Green (1);
        mem.(13 * 4) <- Green (1);
        mem.(14 * 4) <- Green (2);
        mem.(15 * 4) <- Green (2);
        mem.(16 * 4) <- Green (0);
        mem.(17 * 4) <- Green (0);
        mem.(18 * 4) <- Green (4);
        mem.(19 * 4) <- Green (2);
        mem.(20 * 4) <- Green (2);
        mem.(21 * 4) <- Green (5);
        mem.(100 * 4) <- Green (100);
        JitUtil.enable_jit := true;
        Logger.log_level := Logger.Debug;
        let jit_args =
          { trace_name = "test_trace.1000";
            reds = ["a.109"; "regs.110"];
            loop_header = 4;
            loop_pc = 108; }
        in
        let trace = exec_jitcompile prog instr reg mem jit_args in
        let prog' = Prog ([], fundef :: trace :: [], main) in
        let reg' = Array.make 10000 0 in
        let mem' = Array.make 10000 0 in
        print_string (EmitVirtual.to_string_fundef trace);
        let _ = Interp.interp (Interp.to_prog_with_label prog') main reg' mem' jit_args in
        ()
      end
    ]
  end
