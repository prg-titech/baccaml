open Base

val run : Asm.prog -> Jit_env.reg -> Jit_env.mem -> Jit_env.env -> Asm.fundef

val run_multi : Asm.prog -> Jit_env.reg -> Jit_env.mem -> Jit_env.env -> Asm.fundef list
