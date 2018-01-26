val find_pc : Core.String.t Core.List.t -> int -> int
val value_of_id_t : 'a Core.Array.t -> Core.String.t -> 'a
val value_of_id_or_imm :
  Jit_config.value Core.Array.t -> Asm.id_or_imm -> Jit_config.value
val name_of : Core.String.t -> Core.String.t
val print_value : Jit_config.value -> unit
type method_jit_result =
    MSpecialized of Jit_config.value
  | MNot_specialized of Asm.exp * Jit_config.value
type method_jit_args = {
  method_name : string;
  reds : string list;
  method_start : int;
  method_end : int;
  pc_place : int;
}
val can_enter_else : bool Core.ref
val method_jit :
  Asm.prog ->
  Asm.t -> Jit_config.value Core.Array.t -> Tracing_jit.mem -> 'a -> Asm.t
val method_jit_ans :
  Asm.prog ->
  Asm.exp -> Jit_config.value Core.Array.t -> Tracing_jit.mem -> 'a -> Asm.t
val exec_method_jit :
  Asm.prog ->
  Asm.t ->
  Jit_config.value Core.Array.t ->
  Tracing_jit.mem -> method_jit_args -> Asm.fundef
