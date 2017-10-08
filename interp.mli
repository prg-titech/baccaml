module ListUtil :
  sig
    val zip : 'a list -> 'b list -> ('a * 'b) list
    val unzip : ('a * 'b) list -> 'a list * 'b list
  end
module Logger : sig val info : string -> unit val debug : string -> unit end
module Exception : sig exception Un_Implemented_Instruction of string end
val int_of_id_t : Id.t -> int
val int_of_id_or_imm : Asm.id_or_imm -> int
val string_of_id_or_imm : Asm.id_or_imm -> Id.t
val lookup : Asm.prog -> Id.l -> Asm.fundef
val lookup_by_id_t : Asm.prog -> Id.t -> Asm.fundef
val make_reg_set : int array -> Id.t list -> Id.t list -> int array
val interp : Asm.prog -> Asm.t -> int array -> int array -> int
val interp' : Asm.prog -> Asm.exp -> int array -> int array -> int
val f : Asm.prog -> unit
