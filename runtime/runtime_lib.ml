open Std
open MinCaml
open Jit

module Log = struct
  include Log

  let time fmt = Printf.fprintf stderr fmt
end

module Util = struct
  open Asm
  module I = Config.Internal

  let get_id elem = List.find (fun arg -> String.get_name arg = elem)

  let filter typ =
    match typ with
    | `Red ->
      List.filter (fun a ->
          List.mem (String.get_name a) !Config.reds)
    | `Green ->
      List.filter (fun a ->
          List.mem (String.get_name a) !Config.greens)
  ;;

  let find_mj_entries bytecode =
    let annot_mj_comp = -1024 in
    List.map
      fst
      (List.find_all
         (fun (i, elem) -> elem = annot_mj_comp)
         (List.mapi (fun i x -> i, x) (Array.to_list bytecode)))
  ;;

  let find_tj_entries bytecode =
    let annot_tj_comp = -1048 in
    List.map
      fst
      (List.find_all
         (fun (i, elem) -> elem = annot_tj_comp)
         (List.mapi (fun i x -> i, x) (Array.to_list bytecode)))
  ;;

  let file_open () =
    match !Config.file_name with
    | Some name -> open_in name
    | None -> failwith "argument is not specified."
  ;;

  let gen_ir () =
    let ic = file_open () in
    try
      let p = ic |> Lexing.from_channel |> Util.virtualize in
      close_in ic;
      p
    with
    | e ->
      close_in ic;
      raise e
  ;;

  let with_jit_flg ~on:f ~off:g =
    match !Config.jit_flag with `On -> f () | `Off -> g ()
  ;;

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

  let is_debug () =
    match !Log.log_level with `Debug -> true | _ -> false
  ;;

  let with_debug f =
    match !Log.log_level with `Debug -> f () | _ -> ()
  ;;
end

module Compat = struct
  let of_bytecode bytecode =
    Array.map (fun x -> if x = -2048 then 0 else x) bytecode
  ;;
end
