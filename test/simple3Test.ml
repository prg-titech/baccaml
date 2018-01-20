open Asm
open Util
open TracingJit
open MethodJit
open JitConfig
open MincamlUtil
open TestUtil

let prog =
  open_in (dir ^ "simple3.ml")
  |> Lexing.from_channel
  |> virtualize

let Prog (_, fundefs, main) = prog

let fundef = List.hd fundefs

let jit_args = {
  trace_name = "test_trace.1000";
  reds = ["stack.112"];
  greens = ["bytecode.110"; "pc.111"; "sp.113"];
  loop_header = 0;
  loop_pc_place = 0
}

let _ =
  let { body } = fundef in
  let reg = Array.make 10000 (Red 0) in
  let mem = Array.make 10000 (Red 0) in
  reg.(111) <- Green 7;
  reg.(112) <- Red 100;
  reg.(113) <- Green 1;
  mem.(0) <- Green 4; mem.(4) <- Green 5;
  mem.(8) <- Green 22; mem.(12) <- Green 2;
  mem.(16) <- Green 0;
  mem.(20) <- Green 11; mem.(24) <- Green 2;
  mem.(28) <- Green 10; mem.(32) <- Green 0;
  mem.(36) <- Green 30;
  mem.(100) <- Red 4; mem.(104) <- Red 5;
  let res = method_jit prog body reg mem jit_args in
  print_string (EmitVirtual.to_string_t res);
  ()
