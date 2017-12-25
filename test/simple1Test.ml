open Asm
open Util
open Jit
open Jit.Util
open MincamlUtil
open TestUtil

let prog =
  open_in (dir ^ "simple1.ml")
  |> Lexing.from_channel
  |> virtualize

let Prog (_, fundefs, main) = prog

let fundef =
  List.hd fundefs

let prepare reg mem =
  mem.(0 * 4) <- Green (1);
  mem.(1 * 4) <- Green (2);
  mem.(2 * 4) <- Green (0);
  mem.(3 * 4) <- Green (4);
  reg.(40) <- Green (0);
  reg.(41) <- Green (0);
  reg.(42) <- Red (100);
  ()

let jit_args =
  { trace_name = "test_trace.1000"
  ; reds = ["a.42"]
  ; greens = []
  ; loop_header = 0
  ; loop_pc_place = 1 }

let _ =
  Arg.parse
    [("-jit", Arg.Unit (fun _ -> Jit.Util.enable_jit := true), "enable jit compile");
     ("-debug", Arg.Unit (fun _ -> Logger.log_level := Logger.Debug), "debug mode");]
    (fun s -> ())
    ("usage: -jit: enable jit, -debug: execute as debug mode");
  let instr = fundef.body in
  let reg, mem = Array.make 1000 (Red 0), Array.make 1000 (Red 0) in
  let reg', mem' = Array.make 10000 0, Array.make 10000 0 in
  prepare reg mem;
  let res = exec_jitcompile prog instr reg mem jit_args in
  let prog' = Prog ([], fundef :: res :: [], main) in
  setup reg reg';
  setup mem mem';
  Logger.log_level := Logger.Debug;
  enable_jit := true;
  print_string (EmitVirtual.to_string_fundef res);
  (*prog' |> Simm.f |> RegAlloc.f |> Emit.f (open_out ("test/simple1.s"));*)
  ()

