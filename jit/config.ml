let file_name = ref None
let jit_flag = ref `On
let reds = ref [ "" ]
let greens = ref [ "" ]
let log_level = Log.log_level
let comp_only_flag = ref `On

let jit_mode =
  Option.fold (Sys.getenv_opt "JIT_MODE") ~none:`Tracing ~some:(fun mode_str ->
      let mode_str = String.lowercase_ascii mode_str in
      if mode_str = "tj" || mode_str = "tracing"
      then `Tracing
      else if mode_str = "mj" || mode_str = "method"
      then `Method
      else if mode_str = "tj|mj" || mode_str = "tracing|method" || mode_str = "all"
      then `All
      else failwith "Invalid runtime environment")
;;

let set field value = field := value
let () = Log.log_level := !log_level
