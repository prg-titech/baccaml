open Std
open MinCaml
open Jit
open Jit_env
open Jit_prof
open Jit_compile
open Opt
open Runtime_env
open Runtime_lib

open Printf

module TJ = Runtime_tracing
module MJ = Runtime_method

let jit_setup_run_once_flg = ref false

let jit_gen_trace bytecode stack pc sp bc_ptr st_ptr =
  let jit_apply f pcs =
    List.iter (fun pc -> f bytecode stack (pc + 1) sp bc_ptr st_ptr) pcs
  in
  let tj_pcs = Util.find_tj_entries bytecode |> List.rev in
  let mj_pcs = Util.find_mj_entries bytecode in
  if not !jit_setup_run_once_flg
  then (
    jit_setup_run_once_flg := true;
    match !Config.jit_setup_mode with
    | `Tracing -> tj_pcs |> jit_apply TJ.jit_tracing_gen_trace
    | `Method -> mj_pcs |> jit_apply MJ.jit_method_gen_trace
    | `All ->
      tj_pcs |> jit_apply TJ.jit_tracing_gen_trace;
      mj_pcs |> jit_apply MJ.jit_method_gen_trace
    | `Nothing -> ())
;;

let register_interp_ir () =
  interp_ir := Some (Util.gen_ir ());
  interp_fundef
    := Option.bind !interp_ir (fun interp_ir ->
           Some (Fundef.find_fuzzy interp_ir "interp"))
;;

let callbacks _ =
  Callback.register "jit_tracing_gen_trace" TJ.jit_tracing_gen_trace;
  Callback.register "jit_tracing_entry" TJ.jit_tracing_entry;
  Callback.register "jit_tracing_exec" TJ.jit_tracing_exec;
  Callback.register "jit_guard_occur_at" TJ.jit_guard_occur_at;
  Callback.register "jit_method_call" MJ.jit_method_call;
  Callback.register "jit_setup" jit_gen_trace;
  register_interp_ir ();
  ()
;;
