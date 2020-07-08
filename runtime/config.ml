let file_name : string option ref = ref None
let reds : string list ref = ref [ "" ]
let greens : string list ref = ref [ "" ]

let jit_flag : [ `On | `Off ] ref = ref `On
let hybrid_flag : [`TJ | `MJ | `Nothing] ref = ref `Nothing
let opt_flag : [`On | `Off] ref = ref `On

let () = Log.log_level := `App

module Deprecated = struct
  let jit_setup_mode : [`Tracing | `Method | `All | `Nothing] ref = ref `Nothing
end

module Internal = struct
  let size = ref (Sys.max_array_length / 2)
  let thold_tj = ref 100
  let thold_guard = ref 100
  let bc_tmp_addr = 0
  let st_tmp_addr = 1024
end
