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

module Util = struct
  open Asm

  let get_id elem = List.find (fun arg -> String.get_name arg = elem)

  let filter typ =
    match typ with
    | `Red ->
      List.filter (fun a -> List.mem (String.get_name a) Internal_conf.reds)
    | `Green ->
      List.filter (fun a -> List.mem (String.get_name a) Internal_conf.greens)
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

  let make_reg prog args sp =
    let open Jit_env in
    let reg = Array.make Internal_conf.size (Red 0) in
    let Asm.{ args; body = t } = Fundef.find_fuzzy prog "interp" in
    Asm.fv t @ args
    |> List.iteri (fun i a ->
           if List.mem (String.get_name a) Internal_conf.greens
           then reg.(i) <- Green 0
           else reg.(i) <- Red 0);
    reg
  ;;

  let make_mem ~bc_addr ~st_addr bytecode stack =
    let open Jit_env in
    let mem = Array.make Internal_conf.size (Green 0) in
    bytecode
    |> Array.iteri (fun i a -> mem.(bc_addr + (4 * i)) <- Jit_env.Green a);
    stack |> Array.iteri (fun i a -> mem.(st_addr + (4 * i)) <- Jit_env.Red a);
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
      ~lib:("./" ^ get_so_name name)
      ~func:(String.split_on_char '.' name |> List.hd)
      ~arg1
      ~arg2
  ;;

  let exec_dyn_arg2_with_elapsed_time ?(notation = None) ~name ~arg1 ~arg2 =
    if Debug.is_debug ()
    then (
      let s = Sys.time () in
      let v = exec_dyn_arg2 name arg1 arg2 in
      let e = Sys.time () in
      (match notation with
      | Some `Tracing ->
        Printf.eprintf "[tj] elapsed time %fus\n" ((e -. s) *. 1e6)
      | Some `Method ->
        Printf.eprintf "[mj] elapsed time %fus\n" ((e -. s) *. 1e6)
      | None -> ());
      flush stderr;
      v)
    else exec_dyn_arg2 name arg1 arg2
  ;;

  let exec_dyn_arg3 ~name ~arg1 ~arg2 ~arg3 =
    Dynload_stub.call_arg3
      ~lib:("./" ^ get_so_name name)
      ~func:(String.split_on_char '.' name |> List.hd)
      ~arg1
      ~arg2
      ~arg3
  ;;
end

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

module Retry = struct
  let traces_tj : Asm.fundef list ref = ref []
end

let interp_ir : Asm.prog option ref = ref None

let jit_method ({ bytecode; stack; pc; sp; bc_ptr; st_ptr } as runtime_env) prog
  =
  let open Asm in
  let open Jit_env in
  let module JM = Jit_method in
  let reg, mem = Setup.env runtime_env `Meta_method prog in
  let { args } = Fundef.find_fuzzy prog "interp" in
  let trace_name = Trace_name.gen `Meta_method in
  let env =
    create_env
      ~trace_name:(Trace_name.value trace_name)
      ~red_names:!Config.reds
      ~index_pc:(List.index (Util.get_id "pc" args) args)
      ~merge_pc:pc
      ~bytecode
  in
  let (`Result (trace, others)) = JM.run prog reg mem env in
  let trace = trace |> Jit_constfold.h |> Opt_defuse.h in
  Debug.with_debug (fun _ -> print_fundef trace);
  Option.fold
    others
    ~none:(emit_and_compile prog `Meta_method trace)
    ~some:(fun others ->
      emit_and_compile_with_so prog `Meta_tracing others trace)
;;

let jit_tracing
    ({ bytecode; stack; pc; sp; bc_ptr; st_ptr } as runtime_env)
    prog
  =
  let open Asm in
  let open Jit_env in
  let module JT = Jit_tracing in
  let reg, mem = Setup.env runtime_env `Meta_tracing prog in
  let { args } = Fundef.find_fuzzy prog "interp" in
  let trace_name = Trace_name.gen `Meta_tracing in
  let env =
    create_env
      ~index_pc:
        (let pc_id = List.find (fun arg -> String.get_name arg = "pc") args in
         List.index pc_id args)
      ~merge_pc:pc
      ~trace_name:(Trace_name.value trace_name)
      ~red_names:!Config.reds
      ~bytecode
  in
  let (`Result (trace, others)) = JT.run prog reg mem env in
  let trace = trace |> Jit_constfold.h |> Opt_defuse.h in
  Retry.traces_tj := !Retry.traces_tj @ [trace];
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

let jit_tracing_exec pc st_ptr sp stack =
  let open Util in
  with_jit_flg
    ~off:(fun _ -> ())
    ~on:(fun _ ->
      match Trace_prof.find_opt pc with
      | Some tname ->
        eprintf "[tj] executing %s at pc: %d sp: %d ...\n" tname pc sp;
        let _ =
          exec_dyn_arg2_with_elapsed_time
            ~notation:(Some `Tracing)
            ~name:tname
            ~arg1:st_ptr
            ~arg2:sp
        in
        flush_all ()
      | None ->
        ())
;;

let jit_method_gen_trace bytecode stack pc sp bc_ptr st_ptr =
  let p = Option.get !interp_ir |> Jit_annot.annotate `Meta_method in
  let bytecode = Compat.of_bytecode bytecode in
  let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
  match p |> jit_method env with
  | Ok name ->
    Debug.with_debug (fun _ -> eprintf "[mj] compiled %s at pc: %d\n" name pc);
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
      Printf.eprintf "[mj] compiled %s at pc: %d\n" name pc;
      let s = Sys.time () in
      let r = exec_dyn_arg2 ~name ~arg1:st_ptr ~arg2:sp in
      let e = Sys.time () in
      Printf.printf "[mj] elapced time: %f us\n" ((e -. s) *. 1e6);
      flush stderr;
      flush stdout;
      r
    | Error e -> raise e)
;;

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
    | `Tracing -> tj_pcs |> jit_apply jit_tracing_gen_trace
    | `Method -> mj_pcs |> jit_apply jit_method_gen_trace
    | `All ->
      tj_pcs |> jit_apply jit_tracing_gen_trace;
      mj_pcs |> jit_apply jit_method_gen_trace
    | `Nothing -> ())
;;

let register_interp_ir () = interp_ir := Some (Util.gen_ir ())

let callbacks () =
  Callback.register "jit_tracing_entry" jit_tracing_entry;
  Callback.register "jit_tracing_exec" jit_tracing_exec;
  Callback.register "jit_method_call" jit_method_call;
  Callback.register "jit_setup" jit_gen_trace;
  register_interp_ir ();
  ()
;;
