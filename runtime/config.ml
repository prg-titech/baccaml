let file_name = ref None
let jit_flag = ref `On
let jit_setup_mode = ref `Nothing
let comp_only_flag = ref `On
let reds = ref [ "" ]
let greens = ref [ "" ]
let log_level = Log.log_level
let set field value = field := value
let () = Log.log_level := !log_level

module Internal = struct
  let size = ref (Sys.max_array_length / 100)
  let thold_tj = ref 100
  let thold_guard = ref 100
  let bc_tmp_addr = 0
  let st_tmp_addr = 2000
end
