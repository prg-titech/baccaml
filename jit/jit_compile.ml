open MinCaml
open Asm
open Printf

let sp = sprintf

exception Compilation_failed of string

let static_dir = "_static"

let create_static_dir _ =
  let mkdir () = Unix.system (sp "mkdir %s" static_dir) |> ignore in
  let rmdir () = Unix.system (sp "rm -rf %s" static_dir) |> ignore in
  try Sys.is_directory static_dir |> ignore with Sys_error _ -> mkdir ()
;;

(* [warn] work only in Linux *)
let get_so_name : string -> string = fun name -> "lib" ^ name ^ ".so"

let create_archive obj_name other_objs =
  let archive_name = "lib" ^ (obj_name |> Filename.remove_extension) ^ ".a" in
  create_static_dir ();
  sp
    "ar rcs _static/%s %s %s"
    archive_name
    obj_name
    (match other_objs with
    | Some other_objs -> String.concat " " other_objs
    | None -> "")
  |> Unix.system
  |> function
  | Unix.WEXITED i when i = 0 -> Ok archive_name
  | _ -> Error (Compilation_failed obj_name)
;;

(* [warn] work only in Linux *)
let compile_dyn trace_name =
  let asm_name = trace_name ^ ".s"
  and obj_name = trace_name ^ ".o"
  and so = get_so_name trace_name in
  (sp "gcc -m32 -c %s" asm_name
  |> Unix.system
  |> function
  | Unix.WEXITED i when i = 0 -> () | _ -> failwith "compilation failed.");
  sp
    "gcc -m32 %s -o %s -shared -fPIC %s -L./_static"
    (if !Log.log_level = `Debug then "-g" else "")
    so
    asm_name
  |> Unix.system
  |> function
  | Unix.WEXITED i when i = 0 ->
    (match create_archive obj_name None with Ok _ -> Ok trace_name | e -> e)
  | _ -> Error (Compilation_failed trace_name)
;;

let compile_dyn_with_so tname others =
  let asm = tname ^ ".s" in
  let so = get_so_name tname in
  let other_objs =
    others |> List.map (fun so -> so ^ ".o") |> String.concat " "
  in
  sp
    "gcc -m32 %s -o %s %s -shared -fPIC -ldl -L./_static %s"
    (if !Log.log_level = `Debug then "-g" else "")
    so
    asm
    other_objs
  |> Unix.system
  |> function
  | Unix.WEXITED i when i = 0 -> Ok tname
  | _ -> Error (Compilation_failed tname)
;;

let compile_stdout tname =
  let so = get_so_name tname in
  ignore
    (Sys.command
       (Printf.sprintf
          "| gcc -x c -m32 -g -DRUNTIME -shared -fPIC -ldl -o %s -"
          so))
;;

let emit_dyn oc typ tname trace =
  try
    match typ with
    | `Meta_tracing -> trace |> Simm.h |> RegAlloc.h |> Jit_emit.emit_tj oc
    | `Meta_method -> trace |> Simm.h |> RegAlloc.h |> Jit_emit.emit_mj oc
  with
  | e ->
    close_out oc;
    raise e
;;

let emit_and_compile typ (trace : fundef) =
  let { name = Id.L trace_name } = trace in
  let oc = open_out (trace_name ^ ".s") in
  try
    emit_dyn oc typ trace_name trace;
    close_out oc;
    compile_dyn trace_name
  with
  | e ->
    close_out oc;
    raise e
;;

let emit_and_compile_with_so typ others (trace : fundef) =
  let { name = Id.L trace_name } = trace in
  let oc = open_out (trace_name ^ ".s") in
  try
    emit_dyn oc typ trace_name trace;
    close_out oc;
    compile_dyn_with_so trace_name others
  with
  | e ->
    close_out oc;
    raise e
;;

let compile_and_register_dyn prog typ (pc : int) (trace : fundef) : unit =
  let open Jit_prof in
  match emit_and_compile typ trace with
  | Ok tname ->
    (match typ with
     | `Meta_tracing -> Trace_prof.register (pc, tname)
     | `Meta_method -> Method_prof.register (pc, tname))
  | Error e -> ()
;;
