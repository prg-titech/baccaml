open MinCaml
open Asm

type rename_guard_env =
  { pc : int
  ; bname : string
  }

val rename : rename_guard_env -> fundef -> fundef
val embed : rename_guard_env -> mtrace:fundef -> btrace:fundef -> fundef
