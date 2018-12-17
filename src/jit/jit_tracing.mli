open MinCaml

type tj_env = { index_pc: int; merge_pc: int; trace_name: string }

val run_while :
  Asm.prog ->
  Jit_config.value array ->
  Jit_config.value array ->
  string ->
  string list ->
  int ->
  int ->
  Asm.fundef
