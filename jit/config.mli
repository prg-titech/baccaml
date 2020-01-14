val file_name : string option ref
val reds : string list ref
val greens : string list ref
val jit_flag : [ `On | `Off ] ref
val jit_setup_mode : [ `Tracing | `Method | `All | `Nothing ] ref
val comp_only_flag : [ `On | `Off ] ref
val log_level : Log.level ref
val set : 'a ref -> 'a -> unit
