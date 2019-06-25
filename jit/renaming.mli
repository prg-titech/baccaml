open Base
type rename_env = Id.t -> Id.t
val counter : int ref
val empty_env : string -> 'a
val extend_env : (string -> string) -> string -> string -> string
val rename_id_or_imm : (Id.t -> Id.t) -> Asm.id_or_imm -> Asm.id_or_imm
val rename_exp : (Id.t -> Id.t) -> Asm.exp -> Asm.exp
val rename_t : (Id.t -> Id.t) -> Asm.t -> Asm.t
val rename_fundef : Asm.fundef -> Asm.fundef
