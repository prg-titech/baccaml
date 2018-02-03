exception Not_supported of string
val zero : string
val tracing_jit :
  Asm.prog ->
  Asm.t -> Jit_config.reg -> Jit_config.mem -> Jit_config.jit_args -> Asm.t
val optimize_exp :
  Asm.prog ->
  Asm.exp -> Jit_config.reg -> Jit_config.mem -> Jit_config.jit_result
val exec_tracing_jit :
  Asm.prog ->
  Asm.t ->
  Jit_config.reg -> Jit_config.mem -> Jit_config.jit_args -> Asm.fundef
