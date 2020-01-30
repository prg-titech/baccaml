open Std
open MinCaml

module Internal_conf = struct
  let size = Sys.max_array_length
  let greens = !Config.greens
  let reds = !Config.reds
  let bc_tmp_addr = 0
  let st_tmp_addr = 1000
end

module Debug = struct
  let print_trace trace =
    match !Log.log_level with
    | `Info ->
      print_string "[trace]\n";
      Asm.print_fundef trace;
      print_newline ()
    | _ -> ()
  ;;

  let print_arr ?notation:(nt = None) f arr =
    if !Log.log_level = `Debug
    then (
      let str = Array.string_of_array f arr in
      match nt with
      | Some s -> Printf.printf "%s %s\n" s str
      | None -> Printf.printf "%s\n" str)
    else ()
  ;;

  let is_debug () = match !Config.log_level with `Debug -> true | _ -> false
  let with_debug f = match !Config.log_level with `Debug -> f () | _ -> ()
end

module Compat = struct
  let of_bytecode bytecode =
    Array.map (fun x -> if x = -2048 then 0 else x) bytecode
  ;;
end
