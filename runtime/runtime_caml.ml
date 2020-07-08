open Std
open MinCaml
open Asm
open Jit
open Jit_env
open Jit_prof
open Opt
open Runtime_lib
open Runtime_env
open Printf
module E = Jit_env
module I = Config.Internal

let traces : Asm.fundef list ref = ref []
let trace_tbl : (int, Asm.fundef) Hashtbl.t = Hashtbl.create 100
let counter = ref 0

let compile_trace trace_name =
  let cmd = sprintf "gcc -m32 -shared -fPIC -c %s" (trace_name ^ ".s") in
  Sys.command cmd |> ignore
;;

let gen_trace_name typ =
  let str =
    match typ with
    | `Meta_tracing -> sprintf "tracetj%d" !counter
    | `Meta_method -> sprintf "tracemj%d" !counter
  in
  incr counter;
  Id.genid str
;;

let update_trace merge_pc trace =
  Hashtbl.find_opt trace_tbl merge_pc
  |> Option.fold
       ~some:(fun trace -> Hashtbl.replace trace_tbl merge_pc trace)
       ~none:()
;;

let append_trace ~merge_pc trace = Hashtbl.add trace_tbl merge_pc trace

let lookup_merge_trace ~guard_pc =
  let open Jit_guard in
  Option.(
    bind (TJ.lookup_opt ~guard_pc) (function `Pc merge_pc ->
        Hashtbl.find_opt trace_tbl merge_pc))
;;

let jit_tracing bytecode stack pc sp bc_ptr st_ptr =
  let prog = Option.get !interp_ir |> Jit_annot.annotate `Meta_tracing in
  let env = create_runtime_env ~bytecode ~stack ~pc ~sp ~bc_ptr ~st_ptr in
  Setup.env
    env
    `Meta_tracing
    (Option.get !interp_fundef |> Jit_annot.annotate_fundef `Meta_tracing);
  let { args } = Option.get !interp_fundef in
  let trace_name = gen_trace_name `Meta_tracing in
  let env =
    create_env
      ~index_pc:
        (let pc_id = List.find (fun arg -> String.get_name arg = "pc") args in
         List.index pc_id args)
      ~merge_pc:pc
      ~current_pc:pc
      ~trace_name
      ~red_names:!Config.reds
      ~bytecode
  in
  let (`Result (trace, deps_opt)) = Jit_tracing.run prog reg mem env in
  let trace =
    match !Config.opt_flag with
    | `On -> trace |> Jit_constfold.h |> Opt_defuse.h
    | `Off -> trace |> Jit_constfold.h
  in
  let deps = Option.fold ~none:[||] ~some:(fun v -> Array.of_list v) deps_opt in
  append_trace pc trace;
  Log.with_debug (fun _ -> print_fundef trace);
  Trace_prof.register (pc, trace_name);
  let oc = open_out (trace_name ^ ".s") in
  try
    trace |> Simm.h |> RegAlloc.h |> Jit_emit.h `Meta_tracing oc;
    close_out oc;
    trace_name, deps, Array.length deps
  with
  | e ->
    close_out oc;
    raise e
;;

let jit_method bytecode stack pc sp bc_ptr st_ptr =
  let prog = Option.get !interp_ir |> Jit_annot.annotate `Meta_method in
  let env = create_runtime_env ~bytecode ~stack ~pc ~sp ~bc_ptr ~st_ptr in
  Setup.env
    env
    `Meta_method
    (Option.get !interp_fundef |> Jit_annot.annotate_fundef `Meta_method);
  let { args } = Option.get !interp_fundef in
  let trace_name = gen_trace_name `Meta_method in
  let env =
    create_env
      ~index_pc:
        (let pc_id = List.find (fun arg -> String.get_name arg = "pc") args in
         List.index pc_id args)
      ~merge_pc:pc
      ~current_pc:pc
      ~trace_name
      ~red_names:!Config.reds
      ~bytecode
  in
  let (`Result (trace, deps_opt)) = Jit_method.run prog reg mem env in
  let trace =
    match !Config.opt_flag with
    | `On -> trace |> Jit_constfold.h |> Opt_defuse.h
    | `Off -> trace |> Jit_constfold.h
  in
  let deps = Option.fold ~none:[||] ~some:(fun v -> Array.of_list v) deps_opt in
  Log.with_debug (fun _ -> print_fundef trace);
  Method_prof.register (pc, trace_name);
  let oc = open_out (trace_name ^ ".s") in
  try
    trace |> Simm.h |> RegAlloc.h |> Jit_emit.h `Meta_method oc;
    close_out oc;
    trace_name, deps, Array.length deps
  with
  | e ->
    close_out oc;
    raise e
;;

let jit_setup_tj bytecode stack pc sp bc_ptr st_ptr =
  Util.find_tj_entries bytecode
  |> List.rev
  |> List.map (fun pc ->
         let ((trace_name, deps, d_size) as result) =
           jit_tracing bytecode stack (pc + 1) sp bc_ptr st_ptr
         in
         compile_trace trace_name;
         result)
;;

let jit_setup_mj bytecode stack pc sp bc_ptr st_ptr =
  Util.find_mj_entries bytecode
  |> List.map (fun pc ->
         let ((trace_name, deps, d_size) as result) =
           jit_method bytecode stack (pc + 1) sp bc_ptr st_ptr
         in
         compile_trace trace_name;
         result)
;;

let callbacks _ =
  Callback.register "caml_jit_tracing" jit_tracing;
  Callback.register "caml_jit_method" jit_method;
  Callback.register "caml_jit_setup_tj" jit_setup_tj;
  Callback.register "caml_jit_setup_mj" jit_setup_mj;
  ()
;;
