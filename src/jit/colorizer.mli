open MinCaml

val colorize_reg :
  (string, int) Hashtbl.t ->
  (string, int) Hashtbl.t ->
  Jit_config.value array ->
  Asm.fundef -> Asm.t -> unit
