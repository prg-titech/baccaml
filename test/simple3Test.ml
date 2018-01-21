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

let method_jit_args = {
  method_name = "test_method.1000";
  reds = [];
  method_start = 0;
  method_end = 0;
  pc_place = 2
}

let _ =
  let { body } = fundef in
  let reg = Array.make 100000 (Red 0) in
  let mem = Array.make 100000 (Red 0) in
  reg.(111) <- Green 7;
  reg.(112) <- Red 100;
  reg.(113) <- Green 1;
  let bytecode = [|4; 5; 22; 2; 22; 2; 0; 11; 2; 10; 0; 30|] in
  for i = 0 to (Array.length bytecode - 1) do
    mem.(i * 4) <- Green (bytecode.(i))
  done;
  let res = method_jit prog body reg mem method_jit_args in
  print_string (EmitVirtual.to_string_t res);
  ()
