open Base
open Jit_env

val restore : reg -> string list -> ?wlist:string list -> Asm.t -> Asm.t

module TJ : sig
  val create : reg -> string -> ?wlist:string list -> Asm.t -> Asm.t
end

module MJ : sig
  val create : reg -> env -> ?wlist:string list -> Asm.t -> Asm.t
end
