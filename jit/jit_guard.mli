open Base
val create : Jit_env.value array -> Jit_env.env-> ?wlist:string list -> Asm.t -> Asm.t
