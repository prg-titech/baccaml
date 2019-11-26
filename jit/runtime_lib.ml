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
       print_string "[trace]\n"; Asm.print_fundef trace; print_newline ()
    | _ -> ()

  let print_arr ?notation:(nt = None) f arr =
    if !Log.log_level = `Debug then
      let str = Array.string_of_array f arr in
      match nt with
      | Some s -> Printf.printf "%s %s\n" s str
      | None -> Printf.printf "%s\n" str
    else ()

  let print_stack stk =
    let str = Array.string_of_array string_of_int stk in
    print_string "[stack] "; print_endline str

  let with_debug = fun f ->
    match !Config.log_level with
    | `Debug -> f ()
    | _ -> ()

end

module Compat = struct
  let of_bytecode bytecode =
    Array.map (fun x -> if x = -1024 then 0 else x) bytecode
end

let file_open () =
  match !Config.file_name with
  | Some name -> open_in name
  | None -> failwith "argument is not specified."

let get_ir_addr args name =
  List.find (fun a -> String.get_name a = name) args
  |> String.get_extension
  |> int_of_string

let make_reg prog args sp =
  let open Jit_env in
  let reg = Array.make Internal_conf.size (Red 0) in
  let Asm.{args; body= t} = Fundef.find_fuzzy prog "interp" in
  Asm.fv t @ args
  |> List.iteri
    (fun i a ->
       if List.mem (String.get_name a) Internal_conf.greens then reg.(i) <- Green 0
       else reg.(i) <- Red 0 ) ;
  reg

let make_mem ~bc_addr ~st_addr bytecode stack =
  let open Jit_env in
  let mem = Array.make Internal_conf.size (Green 0) in
  bytecode
  |> Array.iteri (fun i a -> mem.(bc_addr + (4 * i)) <- Jit_env.Green a) ;
  stack
  |> Array.iteri (fun i a -> mem.(st_addr + (4 * i)) <- Jit_env.Red a) ;
  mem

let with_jit_flg ~on:f ~off:g =
  match !Config.jit_flag with
  | `On -> f ()
  | `Off -> g ()

let with_comp_flg ~on:f ~off:g =
  match !Config.comp_only_flag with
  | `On -> f ()
  | `Off -> g ()
