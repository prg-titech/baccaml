open Std
open MinCaml
open Jit
open Jit_env
open Jit_prof
open Jit_compile
open Opt
open Runtime_lib
open Printf

type runtime_env =
  { bytecode : int array
  ; stack : int array
  ; pc : int
  ; sp : int
  ; bc_ptr : int
  ; st_ptr : int
  }

module Setup = struct
  let env { bytecode; stack; pc; sp; bc_ptr; st_ptr } typ prog =
    let open Asm in
    let open Util in
    let prog = Jit_annot.annotate typ prog
    and { args; body } = Fundef.find_fuzzy prog "interp" in
    let reg = make_reg prog args sp
    and mem =
      Internal_conf.(
        make_mem ~bc_addr:bc_tmp_addr ~st_addr:st_tmp_addr bytecode stack)
    and pc_method_entry = pc
    and pc_ir_addr = get_ir_addr args "pc"
    and sp_ir_addr = get_ir_addr args "sp"
    and bc_ir_addr = get_ir_addr args "bytecode"
    and st_ir_addr = get_ir_addr args "stack" in
    let module E = Jit_env in
    reg.(pc_ir_addr) <- E.Green pc_method_entry;
    reg.(sp_ir_addr) <- E.Red sp;
    reg.(bc_ir_addr) <- E.Green Internal_conf.bc_tmp_addr;
    reg.(st_ir_addr) <- E.Red Internal_conf.st_tmp_addr;
    reg, mem
  ;;
end

let interp_ir : Asm.prog option ref = ref None

module TJ = struct
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
      prog
    =
    let open Asm in
    let open Jit_env in
    let reg, mem = Setup.env runtime_env `Meta_tracing prog in
    let { args } = Fundef.find_fuzzy prog "interp" in
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
      ~none:(emit_and_compile prog `Meta_method trace)
      ~some:(fun others ->
        emit_and_compile_with_so prog `Meta_tracing others trace)
  ;;

  let jit_tracing_gen_trace bytecode stack pc sp bc_ptr st_ptr =
    let open Util in
    let prog = Option.get !interp_ir |> Jit_annot.annotate `Meta_tracing in
    let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
    match prog |> jit_tracing env with
    | Ok name -> Trace_prof.register (pc, name)
    | Error e -> ()
  ;;

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
        else Trace_prof.count_up pc;
        ())
  ;;

  let jit_tracing_retry
      ({ bytecode; stack; pc; sp; bc_ptr; st_ptr } as runtime_env)
    =
    let prog = Option.get !interp_ir |> Jit_annot.annotate `Meta_tracing in
    let open Asm in
    let open Jit_env in
    let open Trace_prof in
    let reg, mem = Setup.env runtime_env `Meta_tracing prog in
    let { args } = Fundef.find_fuzzy prog "interp" in
    let bridge_name = Trace_name.gen `Meta_tracing in
    let env =
      create_env
        ~index_pc:
          (List.mapi (fun i x -> i, x) args
          |> List.find (fun (i, arg) -> String.get_name arg = "pc")
          |> fst)
        ~merge_pc:pc
        ~current_pc:pc
        ~trace_name:(Trace_name.value bridge_name)
        ~red_names:!Config.reds
        ~bytecode
    in
    let (`Result (trace, others)) = Jit_tracing.run prog reg mem env in
    let bridge_trace =
      trace |> Jit_constfold.h |> Opt_defuse.h |> Renaming.rename_fundef
    in
    Debug.with_debug (fun _ -> print_fundef bridge_trace);
    Guard.register_name (pc, Trace_name.value bridge_name);
    match others with
    | Some others ->
      begin
        match
          emit_and_compile_with_so prog `Meta_tracing others bridge_trace
        with
        | Ok bname ->
          begin
            match lookup_merge_trace pc with
            | Some mtrace ->
              let mtrace' =
                Opt_retry.rename
                  { pc; bname = Trace_name.value bridge_name }
                  mtrace
              in
              begin
                match
                  emit_and_compile_with_so prog `Meta_tracing [ bname ] mtrace'
                with
                | Ok mname_compiled' -> ()
                | Error e -> ()
              end
            | None -> ()
          end
        | Error e -> ()
      end
    | None -> ()
  ;;

  let jit_guard_occur_at bytecode stack pc sp bc_ptr st_ptr =
    let open Trace_prof in
    Guard.count_up pc;
    if Guard.over_threshold pc
    then
      if not (Guard.mem_name pc)
      then begin
        let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
        jit_tracing_retry env
      end
  ;;

  let jit_tracing_exec pc st_ptr sp stack =
    let open Util in
    with_jit_flg
      ~off:(fun _ -> ())
      ~on:(fun _ ->
        match Trace_prof.find_opt pc with
        | Some tname ->
          Log.debug (sprintf "executing %s at pc: %d sp: %d ..." tname pc sp);
          let _ =
            exec_dyn_arg2_with_elapsed_time
              ~notation:(Some `Tracing)
              ~name:tname
              ~arg1:st_ptr
              ~arg2:sp
          in
          flush_all ()
        | None -> ())
  ;;
end

module MJ = struct
  let jit_method
      ({ bytecode; stack; pc; sp; bc_ptr; st_ptr } as runtime_env)
      prog
    =
    let open Asm in
    let open Jit_env in
    let reg, mem = Setup.env runtime_env `Meta_method prog in
    let { args } = Fundef.find_fuzzy prog "interp" in
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
      ~none:(emit_and_compile prog `Meta_method trace)
      ~some:(fun others ->
        emit_and_compile_with_so prog `Meta_tracing others trace)
  ;;

  let jit_method_gen_trace bytecode stack pc sp bc_ptr st_ptr =
    let p = Option.get !interp_ir |> Jit_annot.annotate `Meta_method in
    let bytecode = Compat.of_bytecode bytecode in
    let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
    match p |> jit_method env with
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
      let s = Sys.time () in
      let r = exec_dyn_arg2 ~name ~arg1:st_ptr ~arg2:sp in
      let e = Sys.time () in
      printf "[mj] elapced time: %f us\n" ((e -. s) *. 1e6);
      flush stdout;
      r
    | None ->
      let p = Option.get !interp_ir |> Jit_annot.annotate `Meta_method in
      let bytecode = Compat.of_bytecode bytecode in
      let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
      (match p |> jit_method env with
      | Ok name ->
        Method_prof.register (pc, name);
        Log.debug @@ sprintf "[mj] compiled %s at pc: %d\n" name pc;
        let s = Sys.time () in
        let r = exec_dyn_arg2 ~name ~arg1:st_ptr ~arg2:sp in
        let e = Sys.time () in
        Printf.printf "[mj] elapced time: %f us\n" ((e -. s) *. 1e6);
        flush stderr;
        flush stdout;
        r
      | Error e -> raise e)
  ;;
end

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

let register_interp_ir () = interp_ir := Some (Util.gen_ir ())

let callbacks () =
  Callback.register "jit_tracing_entry" TJ.jit_tracing_entry;
  Callback.register "jit_tracing_exec" TJ.jit_tracing_exec;
  Callback.register "jit_guard_occur_at" TJ.jit_guard_occur_at;
  Callback.register "jit_method_call" MJ.jit_method_call;
  Callback.register "jit_setup" jit_gen_trace;
  register_interp_ir ();
  ()
;;
