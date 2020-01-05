let file_name = ref None
let pc_method_annot_inst = 15
let jit_flag = ref `On
let reds = ref [ "" ]
let greens = ref [ "" ]
let log_level = Log.log_level
let comp_only_flag = ref `On
let set field value = field := value
let () = Log.log_level := !log_level
