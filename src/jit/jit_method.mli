open MinCaml

(* run method jit compilation *)
val run_while : Asm.prog -> Jit_util.reg -> Jit_util.mem -> M.key -> string list -> Asm.fundef list
