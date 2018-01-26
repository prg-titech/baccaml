exception Not_supported of string
type reg = Jit_config.value array
type mem = Jit_config.value array
val bac_caml_nop_id : string
module Guard :
  sig
    val select_branch : Asm.exp -> 'a -> 'a -> 'b -> 'b -> 'b
    val get_free_vars : Asm.t -> Id.t Core.List.t
    val get_free_vars' : Asm.exp -> Id.t Core.List.t
    val restore_green : Jit_config.value Core.Array.t -> Asm.t -> Asm.t
  end
val add_cont_proc : Id.t -> Asm.t -> Asm.t -> Asm.t
val tracing_jit :
  Asm.prog -> Asm.t -> reg -> mem -> Jit_config.jit_args -> Asm.t
val tracing_jit_ans :
  Asm.prog -> Asm.exp -> reg -> mem -> Jit_config.jit_args -> Asm.t
val optimize_exp : Asm.prog -> Asm.exp -> reg -> mem -> Jit_config.jit_result
val exec_tracing_jit :
  Asm.prog -> Asm.t -> reg -> mem -> Jit_config.jit_args -> Asm.fundef
