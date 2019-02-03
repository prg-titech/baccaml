type level = [
  | `Error
  | `Debug
  | `Warn
  | `App
  ]

let log_level : level ref = ref `Error

let error s =
  match !log_level with
  | `Error -> Printf.printf "[ERROR] %s\n" s
  | _ -> ()

let debug s =
  match !log_level with
  | `Debug -> Printf.printf "[DEBUG] %s\n" s
  | _ -> ()

let warn s =
  match !log_level with
  | `Warn -> Printf.printf "[WARN] %s\n" s
  | _ -> ()

let app s =
  match !log_level with
  | `App -> Printf.printf "[APP] %s\n" s
  | _ -> ()
