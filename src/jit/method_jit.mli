val method_jit :
  Asm.prog ->
  Asm.t ->
  Jit_config.reg -> Jit_config.mem -> Jit_config.method_jit_args -> Asm.t
val exec_method_jit :
  Asm.prog ->
  Asm.t ->
  Jit_config.reg ->
  Jit_config.mem -> Jit_config.method_jit_args -> Asm.fundef
