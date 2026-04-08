(* Architecture-specific constants for x86-64 *)

let word_size = 8
let int_size = 8
let float_size = 8
let pointer_size = 8

let align i = if i mod 16 = 0 then i else i + 8

(* JIT mode constants used in interpreter bytecode *)
let mode_method = 100
let mode_tracing = 200
