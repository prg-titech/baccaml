open MinCaml

(* run method jit compilation *)
val run_while : Asm.prog -> Jit_config.reg -> Jit_config.mem -> M.key -> string list -> Asm.fundef list
