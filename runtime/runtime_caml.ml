open Std
open MinCaml
open Asm
open Jit
open Jit_env
open Jit_prof
open Opt
open Printf
module E = Jit_env
module I = Config.Internal
open Runtime_lib
open Runtime_env

let traces : Asm.fundef list ref = ref []
let trace_tbl : (int, Asm.fundef) Hashtbl.t = Hashtbl.create 100
let c_tracing = ref 0
let c_method = ref 0

let gen_trace_name = function
  | `Meta_tracing ->
    let str = sprintf "tracetj%d" !c_tracing in
    incr c_tracing;
    Id.genid str
  | `Meta_method ->
    let str = sprintf "tracemj%d" !c_method in
    incr c_method;
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
  let trace = trace |> Jit_constfold.h |> Opt_defuse.h in
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
  let trace = trace |> Jit_constfold.h |> Opt_defuse.h in
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

let callbacks _ =
  Callback.register "caml_jit_tracing" jit_tracing;
  Callback.register "caml_jit_method" jit_method;
  ()
;;
