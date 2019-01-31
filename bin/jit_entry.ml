open Bc_jit
open Jit_util

let dummy_fun x = print_int x ; print_newline ()

let jit_entry bytecode stack pc sp =
  let size = 10000 in
  let reg = Array.make size (Red 0) in
  let mem = Array.make size (Green 0) in
  ()

let () = Callback.register "dummy_fun" dummy_fun
