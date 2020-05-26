open Std
open MinCaml
open Jit

module Log = struct
  include Log

  let oc_time = open_out "elapsed_time.log"
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

  let get_ir_addr args name =
    List.find (fun a -> String.get_name a = name) args
    |> String.get_extension
    |> int_of_string
  ;;

  let make_reg { args; body = t } =
    let open Jit_env in
    let reg = Array.make !I.size (Red 0) in
    Asm.fv t @ args
    |> List.iteri (fun i a ->
           if List.mem (String.get_name a) !Config.greens
           then reg.(i) <- Green 0
           else reg.(i) <- Red 0);
    reg
  ;;

  let make_mem ~bc_addr ~st_addr bytecode stack =
    let open Jit_env in
    let mem = Array.make !I.size (Green 0) in
    bytecode
    |> Array.iteri (fun i a -> mem.(bc_addr + (4 * i)) <- Jit_env.Green a);
    stack
    |> Array.iteri (fun i a -> mem.(st_addr + (4 * i)) <- Jit_env.Red a);
    mem
  ;;

  let with_jit_flg ~on:f ~off:g =
    match !Config.jit_flag with `On -> f () | `Off -> g ()
  ;;

  let with_comp_flg ~on:f ~off:g =
    match !Config.comp_only_flag with `On -> f () | `Off -> g ()
  ;;

  let exec_dyn_arg2 ~name ~arg1 ~arg2 =
    Dynload_stub.call_arg2
      ~lib:("./" ^ Jit_compile.get_so_name name)
      ~func:(String.split_on_char '.' name |> List.hd)
      ~arg1
      ~arg2
  ;;

  let exec_dyn_arg2_with_elapsed_time ?(notation = None) ~name ~arg1 ~arg2 =
    let s = Sys.time () in
    let v = exec_dyn_arg2 name arg1 arg2 in
    let e = Sys.time () in
    (match notation with
    | Some `Tracing -> Log.time "[tj] elapsed time %fus\n" ((e -. s) *. 1e6)
    | Some `Method -> Log.time "[mj] elapsed time %fus\n" ((e -. s) *. 1e6)
    | None -> ());
    flush stderr;
    v
  ;;

  let exec_dyn_arg3 ~name ~arg1 ~arg2 ~arg3 =
    Dynload_stub.call_arg3
      ~lib:("./" ^ Jit_compile.get_so_name name)
      ~func:(String.split_on_char '.' name |> List.hd)
      ~arg1
      ~arg2
      ~arg3
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
    match !Config.log_level with `Debug -> true | _ -> false
  ;;

  let with_debug f =
    match !Config.log_level with `Debug -> f () | _ -> ()
  ;;
end

module Compat = struct
  let of_bytecode bytecode =
    Array.map (fun x -> if x = -2048 then 0 else x) bytecode
  ;;
end
