open Mincaml
val find_loop : Asm.prog -> Asm.t -> Jit_config.reg -> Jit_config.mem -> Jit_config.jit_args-> Asm.t
val exec : Asm.prog -> Asm.t -> Jit_config.reg -> Jit_config.mem -> Jit_config.jit_args -> Jit_config.trace_result
