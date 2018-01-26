exception Not_supported of string
val bac_caml_nop_id : string
val find_pc : Core.String.t Core.List.t -> Jit_config.jit_args -> int
val add_cont_proc : Id.t -> Asm.t -> Asm.t -> Asm.t
val tracing_jit :
  Asm.prog ->
  Asm.t -> Jit_config.reg -> Jit_config.mem -> Jit_config.jit_args -> Asm.t
val tracing_jit_ans :
  Asm.prog ->
  Asm.exp -> Jit_config.reg -> Jit_config.mem -> Jit_config.jit_args -> Asm.t
val optimize_exp :
  Asm.prog ->
  Asm.exp -> Jit_config.reg -> Jit_config.mem -> Jit_config.jit_result
val exec_tracing_jit :
  Asm.prog ->
  Asm.t ->
  Jit_config.reg -> Jit_config.mem -> Jit_config.jit_args -> Asm.fundef
