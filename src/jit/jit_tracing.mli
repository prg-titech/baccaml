open MinCaml

type tj_env = { index_pc: int; merge_pc: int; trace_name: string }

val run_while :
  Asm.prog ->
  Jit_util.value array ->
  Jit_util.value array ->
  string ->
  string list ->
  int ->
  int ->
  Asm.fundef
