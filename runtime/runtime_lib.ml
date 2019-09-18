open Std
open Base
open Jit

exception Jit_compilation_failed

module type Prof = sig
  val threshold : int
end

module Make_prof (M_prof : Prof) = struct
  open M_prof

  type pc = int
  type name = string
  let count_tbl = Hashtbl.create 10000
  let compiled_tbl = Hashtbl.create 10000

  let register : pc * name -> unit = fun (pc, name) ->
    match Hashtbl.find_opt compiled_tbl pc with
    | Some name -> ()
    | None -> Hashtbl.add compiled_tbl pc name

  let count_up : pc -> unit = fun pc ->
    match Hashtbl.find_opt count_tbl pc with
    | Some count -> Hashtbl.replace count_tbl pc (count + 1)
    | None -> Hashtbl.add count_tbl pc 1

  let find_opt : pc -> name option = fun pc ->
    Hashtbl.find_opt compiled_tbl pc

  let over_threshold : pc -> bool = fun pc ->
    match Hashtbl.find_opt count_tbl pc with
    | Some count -> count >= threshold
    | None -> false
end


module Trace_name : sig
  type t = Trace_name of string

  val gen : [< `Meta_tracing | `Meta_method] -> t
  val value : t -> string
end = struct
  type t = Trace_name of string

  let counter = ref 0

  let gen typ =
    let mark = match typ with
        `Meta_tracing -> "tj"
      | `Meta_method -> "mj"
    in
    let name = "trace" ^ mark ^ string_of_int !counter in
    incr counter;
    Trace_name (Id.genid name)

  let value = function Trace_name s -> s
end

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
    print_string "[stack]"; print_endline str

  let with_debug = fun f ->
    match !Config.log_level with
    | `Debug -> f ()
    | _ -> ()

end

let file_open () =
  match !Config.file_name with
  | Some name -> open_in name
  | None -> failwith "argument is not specified."

let get_ir_addr args name =
  List.find (fun a -> String.get_name a = name) args
  |> String.get_extension
  |> int_of_string

(* [warn] work only in Linux *)
let get_so_name : string -> string =
  fun name -> "lib" ^ name ^ ".so"

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

(* [warn] work only in Linux *)
let compile_dyn trace_name =
  let asm_name = trace_name ^ ".s" in
  let so = get_so_name trace_name in
  Printf.sprintf
    "gcc -m32 -g -DRUNTIME -o %s %s -shared -fPIC -ldl"
    so asm_name
  |> Unix.system
  |> function
  | Unix.WEXITED (i) when i = 0 -> Ok trace_name
  | _ -> Error (Jit_compilation_failed)

let emit_dyn oc p typ tname trace =
  try
    match typ with
    | `Meta_tracing ->
      trace |> Simm.h |> RegAlloc.h |> Jit_emit.emit_tj oc
    | `Meta_method ->
      trace |> Simm.h |> RegAlloc.h |> Jit_emit.emit_mj oc
  with e -> close_out oc; raise e

let with_jit_flg ~on:f ~off:g =
  match !Config.jit_flag with
  | `On -> f ()
  | `Off -> g ()

let with_comp_flg ~on:f ~off:g =
  match !Config.comp_only_flag with
  | `On -> f ()
  | `Off -> g ()
