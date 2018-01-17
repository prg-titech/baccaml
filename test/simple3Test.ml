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

let _ =
  let instr = fundef.body in
  let reg = Array.make 100 (Red 0) in
  let mem = Array.make 100 (Red 0) in
  ()
