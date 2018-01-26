val to_string_idl : Id.l -> string
val to_string_id_or_imm : Asm.id_or_imm -> Id.t
val to_string_type_list : Type.t list -> string
val to_string_type : Type.t -> string
val to_string_ids : string list -> string
val to_string_exp : Asm.exp -> string
val to_string_t : Asm.t -> string
val to_string_fundef : Asm.fundef -> string
val to_string_floating_point_table : (Id.l * float) list -> string
val to_string_prog : Asm.prog -> string
val to_string_labels : (Id.l * int) list -> string list
val g : out_channel -> Asm.prog -> unit
val f : out_channel -> Asm.prog -> unit
