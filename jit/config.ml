let file_name = ref None
let jit_flag = ref `On
let jit_setup_mode = ref `Nothing
let comp_only_flag = ref `On
let reds = ref [ "" ]
let greens = ref [ "" ]
let log_level = Log.log_level
let set field value = field := value
let () = Log.log_level := !log_level
