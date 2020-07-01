open MinCaml
open Jit_env

val restore : reg -> args:string list -> Asm.t -> Asm.t
val promote : reg -> trace_name:string -> Asm.t -> Asm.t

module TJ : sig
  val lookup : guard_pc:int -> [ `Pc of int ]
  val lookup_opt : guard_pc:int -> [ `Pc of int ] option
  val create : reg -> env -> ?wlist:string list -> Asm.t -> Asm.t
end

module MJ : sig
  val create : reg -> env -> ?wlist:string list -> Asm.t -> Asm.t
end
