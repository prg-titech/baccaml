open Base

val colorize_reg :
  (string, int) Hashtbl.t ->
  (string, int) Hashtbl.t ->
  Jit_util.value array ->
  Asm.fundef -> Asm.t -> unit
