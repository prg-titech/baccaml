open Std
open MinCaml
open Jit
open Jit_env
open Jit_prof
open Jit_compile
open Opt
open Printf
open Runtime_env
open Runtime_lib

let traces : Asm.fundef list ref = ref []
let trace_tbl : (int, Asm.fundef) Hashtbl.t = Hashtbl.create 100

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

let jit_tracing
    ({ bytecode; stack; pc; sp; bc_ptr; st_ptr } as runtime_env)
    (prog, interp)
  =
  let open Asm in
  let open Jit_env in
  Setup.env runtime_env `Meta_tracing interp;
  let { args } = Option.get !interp_fundef in
  let trace_name = Trace_name.gen `Meta_tracing in
  let env =
    create_env
      ~index_pc:
        (let pc_id = List.find (fun arg -> String.get_name arg = "pc") args in
         List.index pc_id args)
      ~merge_pc:pc
      ~current_pc:pc
      ~trace_name:(Trace_name.value trace_name)
      ~red_names:!Config.reds
      ~bytecode
  in
  let (`Result (trace, others)) = Jit_tracing.run prog reg mem env in
  let trace = trace |> Jit_constfold.h |> Opt_defuse.h in
  append_trace pc trace;
  Debug.with_debug (fun _ -> print_fundef trace);
  Option.fold
    others
    ~none:(emit_and_compile `Meta_method trace)
    ~some:(fun others -> emit_and_compile_with_so `Meta_tracing others trace)
;;

let jit_tracing_gen_trace bytecode stack pc sp bc_ptr st_ptr =
  let open Util in
  let prog = Option.get !interp_ir |> Jit_annot.annotate `Meta_tracing in
  let interp =
    Option.get !interp_fundef |> Jit_annot.annotate_fundef `Meta_tracing
  in
  let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
  match (prog, interp) |> jit_tracing env with
  | Ok name -> Trace_prof.register (pc, name)
  | Error e -> ()
;;

let traced = Array.make 200 (0, false)

let jit_tracing_entry bytecode stack pc sp bc_ptr st_ptr =
  let open Util in
  with_jit_flg
    ~off:(fun _ -> ())
    ~on:(fun _ ->
      if Trace_prof.over_threshold pc
      then (
        match Trace_prof.find_opt pc with
        | Some _ -> ()
        | None -> jit_tracing_gen_trace bytecode stack pc sp bc_ptr st_ptr)
      else Trace_prof.count_up pc)
;;

let jit_tracing_exec pc st_ptr sp stack =
  let open Util in
  with_jit_flg
    ~off:(fun _ -> ())
    ~on:(fun _ ->
      match Trace_prof.find_opt pc with
      | Some tname ->
        (* Log.debug (sprintf "executing %s at pc: %d sp: %d ..." tname pc sp); *)
        let _ =
          exec_dyn_arg2_time
            ~notation:(Some `Tracing)
            ~name:tname
            ~arg1:st_ptr
            ~arg2:sp
        in
        ()
      | None -> ())
;;

let jit_tracing_retry
    ({ bytecode; stack; pc; sp; bc_ptr; st_ptr } as runtime_env)
    (prog, interp)
  =
  let open Asm in
  let open Jit_env in
  let open Trace_prof in
  Setup.env runtime_env `Meta_tracing interp;
  let { args } = interp in
  let bridge_name = Trace_name.(gen `Meta_tracing |> value) in
  let env =
    create_env
      ~index_pc:
        (List.mapi (fun i x -> i, x) args
        |> List.find (fun (i, arg) -> String.get_name arg = "pc")
        |> fst)
      ~merge_pc:pc
      ~current_pc:pc
      ~trace_name:bridge_name
      ~red_names:!Config.reds
      ~bytecode
  in
  let (`Result (bridge_trace, bridge_others)) =
    Jit_tracing.run prog reg mem env
  in
  let bridge_trace =
    bridge_trace |> Jit_constfold.h |> Opt_defuse.h |> Renaming.rename_fundef
  in
  Debug.with_debug (fun _ -> print_fundef bridge_trace);
  Guard.register_name (pc, bridge_name);
  Option.bind (lookup_merge_trace ~guard_pc:pc) (fun mtrace ->
      Opt_retry.embed { pc; bname = bridge_name } ~mtrace ~btrace:bridge_trace
      |> Jit_constfold.h
      |> Opt_defuse.h
      |> fun embedded_mtrace ->
      Debug.with_debug (fun _ ->
          print_endline "[tj] embedded trace:";
          print_fundef embedded_mtrace;
          print_newline ());
      (match bridge_others with
      | Some others ->
        emit_and_compile_with_so `Meta_tracing others embedded_mtrace
      | None -> emit_and_compile `Meta_tracing embedded_mtrace)
      |> Result.to_option)
  |> Option.fold ~some:(fun _ -> ()) ~none:()
;;

let jit_guard_occur_at bytecode stack pc sp bc_ptr st_ptr =
  let open Trace_prof in
  Guard.count_up pc;
  if Guard.over_threshold pc
  then
    if not (Guard.mem_name pc)
    then begin
      let prog, interp =
        Option.(
          ( get !interp_ir |> Jit_annot.annotate `Meta_tracing
          , get !interp_fundef |> Jit_annot.annotate_fundef `Meta_tracing ))
      in
      let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
      (prog, interp) |> jit_tracing_retry env;
      ()
    end
;;
