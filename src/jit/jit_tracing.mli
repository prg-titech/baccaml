open MinCaml

val run_while :
  Asm.prog ->
  Jit_config.value array ->
  Jit_config.value array ->
  string ->
  string list ->
  int ->
  int ->
  Asm.fundef
