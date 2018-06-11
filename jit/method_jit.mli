open Mincaml
val exec : Asm.prog -> Asm.t -> Jit_config.reg -> Jit_config.mem -> Jit_config.jit_args -> Jit_config.trace_result
