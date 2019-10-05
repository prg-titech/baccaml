open Base

val create : Jit_env.value array -> Jit_env.env-> ?wlist:string list -> Asm.t -> Asm.t

module TJ : sig
  val create : Jit_env.value array -> string -> ?wlist:string list -> Asm.t -> Asm.t
end
