open Mincaml
exception Not_supported of string
val tracing_jit :
  Asm.prog ->
  Asm.t -> Jit_config.reg -> Jit_config.mem -> Jit_config.jit_args -> Asm.t
val exec_tracing_jit :
  Asm.prog ->
  Asm.t ->
  Jit_config.reg -> Jit_config.mem -> Jit_config.jit_args -> Asm.fundef