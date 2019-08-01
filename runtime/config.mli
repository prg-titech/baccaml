val file_name : string option ref
val pc_method_annot_inst: int
val reds : string list ref
val greens : string list ref
val jit_flag : [`On | `Off] ref
val only_compile_flag : [`On | `Off] ref
val log_level : Log.level ref
val set : 'a ref -> 'a -> unit
