let file_name = ref None

let size = 1000000

(* TODO: specify extenally *)
let greens = ["pc"; "bytecode"]

let bc_tmp_addr = 0

let st_tmp_addr = 100

let pc_method_annot_inst = 15

let jit_flag = ref `On

let set_log_level lvl =
  Utils.Log.log_level := lvl
