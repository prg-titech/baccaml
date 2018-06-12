type level =
  | Error
  | Warn
  | Info
  | Debug

  let log_level = ref Error

  let info s = match !log_level with
    | Info | Warn | Debug -> print_string ("[INFO] " ^ s ^ "\n")
    | _ -> ()

  let debug s = match !log_level with
    | Debug -> print_string ("[DEBUG] " ^ s ^ "\n")
    | _ -> ()

  let error s =
    print_string ("[ERROR] " ^ s ^ "\n")
