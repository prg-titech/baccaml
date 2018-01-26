val find_pc : Core.String.t Core.List.t -> int -> int
val value_of_id_t : 'a Core.Array.t -> Core.String.t -> 'a
val value_of_id_or_imm :
  Jit_config.value Core.Array.t -> Asm.id_or_imm -> Jit_config.value
val name_of : Core.String.t -> Core.String.t
val print_value : Jit_config.value -> unit
val method_jit :
  Asm.prog ->
  Asm.t ->
  Jit_config.reg -> Jit_config.mem -> Jit_config.method_jit_args -> Asm.t
val method_jit_ans :
  Asm.prog ->
  Asm.exp ->
  Jit_config.reg -> Jit_config.mem -> Jit_config.method_jit_args -> Asm.t
val exec_method_jit :
  Asm.prog ->
  Asm.t ->
  Jit_config.reg ->
  Jit_config.mem -> Jit_config.method_jit_args -> Asm.fundef
