open MinCaml

type mj_env = {
  trace_name : string;
  red_args : string list;
  index_pc : int;
  merge_pc : int;
}

(* run method jit compilation *)
val run_while : Asm.prog -> Jit_util.reg -> Jit_util.mem -> M.key -> string list -> Asm.fundef list

val run : Asm.prog -> Jit_util.reg -> Jit_util.mem -> mj_env -> Asm.fundef list
