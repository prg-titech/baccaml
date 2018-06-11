open Mincaml
val select_branch : Asm.exp -> 'a -> 'a -> 'b -> 'b -> 'b
val restore_green : Jit_config.value Core.Array.t -> Asm.t -> Asm.t
val restore_green_reg : Jit_config.value Core.Array.t -> Asm.t -> Asm.t
val restore_green_mem : Jit_config.value Core.Array.t -> Asm.t -> Asm.t
