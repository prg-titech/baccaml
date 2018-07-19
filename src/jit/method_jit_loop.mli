open MinCaml
val prep : prog:Asm.prog -> name:string -> red_args:string list -> Asm.fundef list * Asm.t * string list
val run_while : Asm.prog -> Jit_config.reg -> Jit_config.mem -> M.key -> string list -> Asm.fundef list
