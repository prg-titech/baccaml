open Base
val create : Jit_env.value array -> Jit_env.env-> ?wlist:string list -> Asm.t -> Asm.t
val create_tj : Jit_env.value array -> Jit_env.env-> ?wlist:string list -> Asm.t -> Asm.t
