type level =
  [ `App
  | `Warn
  | `Info
  | `Debug
  | `Error
  ]

let log_level : level ref = ref `Error

let error s =
  match !log_level with
  | `Error ->
    Printf.fprintf stderr "[err] %s\n" s;
    flush stderr
  | _ -> ()
;;

let debug s =
  match !log_level with
  | `Debug ->
    Printf.fprintf stderr "[debug] %s\n" s;
    flush stderr
  | _ -> ()
;;

let info s =
  match !log_level with
  | `Debug | `Info ->
    Printf.fprintf stdout "[info] %s\n" s;
    flush stdout
  | _ -> ()
;;

let warn s =
  match !log_level with
  | `Error | `Warn ->
    Printf.fprintf stdout "[warn] %s\n" s;
    flush stdout
  | _ -> ()
;;

let app s =
  match !log_level with
  | `Error | `Warn | `App ->
    Printf.fprintf stdout "[app] %s\n" s;
    flush stdout
  | _ -> ()
;;
