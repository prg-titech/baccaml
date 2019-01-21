open MinCaml

type tj_env = {
  trace_name : string;
  red_args : string list;
  index_pc : int;
  merge_pc : int;
}

val run : Asm.prog -> Jit_util.reg -> Jit_util.mem -> tj_env -> Asm.fundef
