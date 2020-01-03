open MinCaml

val run : Asm.prog -> Jit_env.reg -> Jit_env.mem -> Jit_env.env -> [`Result of Asm.fundef * string list option]
