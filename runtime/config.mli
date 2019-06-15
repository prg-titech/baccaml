val file_name : string option ref
val size : int
val greens : string list
val bc_tmp_addr : int
val st_tmp_addr : int
val pc_method_annot_inst: int
val jit_flag : [`On | `Off] ref
val set_log_level : Utils.Log.level -> unit
