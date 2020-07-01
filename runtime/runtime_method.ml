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

let jit_method
    ({ bytecode; stack; pc; sp; bc_ptr; st_ptr } as runtime_env)
    (prog, interp)
  =
  let open Asm in
  let open Jit_env in
  Setup.env runtime_env `Meta_method interp;
  let { args } = interp in
  let trace_name = Trace_name.gen `Meta_method in
  let env =
    create_env
      ~trace_name:(Trace_name.value trace_name)
      ~red_names:!Config.reds
      ~index_pc:(List.index (Util.get_id "pc" args) args)
      ~merge_pc:pc
      ~current_pc:pc
      ~bytecode
  in
  let (`Result (trace, others)) = Jit_method.run prog reg mem env in
  let trace = trace |> Jit_constfold.h |> Opt_defuse.h in
  Debug.with_debug (fun _ -> print_fundef trace);
  Option.fold
    others
    ~none:(emit_and_compile `Meta_method trace)
    ~some:(fun others -> emit_and_compile_with_so `Meta_tracing others trace)
;;

let jit_method_gen_trace bytecode stack pc sp bc_ptr st_ptr =
  let p, i =
    Option.(
      ( get !interp_ir |> Jit_annot.annotate `Meta_method
      , get !interp_fundef |> Jit_annot.annotate_fundef `Meta_method ))
  in
  let bytecode = Compat.of_bytecode bytecode in
  let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
  match (p, i) |> jit_method env with
  | Ok name ->
    Debug.with_debug (fun _ ->
        Log.debug (sprintf "[mj] compiled %s at pc: %d\n" name pc));
    Method_prof.register (pc, name)
  | Error e -> raise e
;;

let jit_method_call bytecode stack pc sp bc_ptr st_ptr =
  let open Util in
  match Method_prof.find_opt pc with
  | Some name ->
    let v =
      exec_dyn_arg2_time ~notation:(Some `Method) ~name ~arg1:st_ptr ~arg2:sp
    in
    flush stderr;
    v
  | None ->
    let p, i =
      Option.(
        ( get !interp_ir |> Jit_annot.annotate `Meta_method
        , get !interp_fundef |> Jit_annot.annotate_fundef `Meta_method ))
    in
    let bytecode = Compat.of_bytecode bytecode in
    let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
    (match (p, i) |> jit_method env with
    | Ok name ->
      Method_prof.register (pc, name);
      let v =
        exec_dyn_arg2_time ~notation:(Some `Method) ~name ~arg1:st_ptr ~arg2:sp
      in
      flush stderr;
      v
    | Error e -> raise e)
;;
