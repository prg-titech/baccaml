val select_branch : Asm.exp -> 'a -> 'a -> 'b -> 'b -> 'b
val get_free_vars : Asm.t -> Id.t Core.List.t
val get_free_vars' : Asm.exp -> Id.t Core.List.t
val restore_green : Jit_config.value Core.Array.t -> Asm.t -> Asm.t
