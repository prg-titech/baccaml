open Std
open MinCaml
open Jit_env
open Jit_prof
open Jit_compile
open Runtime_lib

type runtime_env =
  { bytecode: int array
  ; stack: int array
  ; pc: int
  ; sp: int
  ; bc_ptr: int
  ; st_ptr: int }

module Util = struct
  open Asm

  let get_id elem = List.find (fun arg -> String.get_name arg = elem)

  let filter typ = match typ with
      `Red ->
      List.filter (fun a -> (List.mem (String.get_name a) Internal_conf.reds))
    | `Green ->
      List.filter (fun a -> List.mem (String.get_name a) Internal_conf.greens)

  let find_mj_entries bytecode =
    let annot_mj_comp = 21 in
    List.map fst
      ((List.find_all(fun (i,elem) -> elem = annot_mj_comp)
          (List.mapi (fun i x -> (i, x)) (Array.to_list bytecode))))

  let%test "find_mj_entries test" =
    let bytecode = Array.init 100 (fun i ->  if i mod 42 = 0 then 21 else i) in
    let expected = bytecode
                   |> Array.to_list
                   |> List.mapi (fun i x -> (i,x))
                   |> List.find_all (fun (i,x) -> x = 21)
                   |> List.map fst in
    find_mj_entries bytecode = expected
end

module Setup = struct
  let env {bytecode; stack; pc; sp; bc_ptr; st_ptr} typ prog =
    let open Asm in
    Debug.print_arr string_of_int bytecode;
    let prog = Jit_annot.annotate typ prog
    and {args; body} = Fundef.find_fuzzy prog "interp" in
    let reg = make_reg prog args sp
    and mem = Internal_conf.(make_mem ~bc_addr:bc_tmp_addr ~st_addr:st_tmp_addr bytecode stack)
    and pc_method_entry = pc
    and pc_ir_addr = get_ir_addr args "pc"
    and sp_ir_addr = get_ir_addr args "sp"
    and bc_ir_addr = get_ir_addr args "bytecode"
    and st_ir_addr = get_ir_addr args "stack" in
    let module E = Jit_env in
    reg.(pc_ir_addr) <- E.Green pc_method_entry ;
    reg.(sp_ir_addr) <- E.Red sp ;
    reg.(bc_ir_addr) <- E.Green Internal_conf.bc_tmp_addr ;
    reg.(st_ir_addr) <- E.Red Internal_conf.st_tmp_addr ;
    reg, mem
end

let jit_method ({bytecode; stack; pc; sp; bc_ptr; st_ptr} as runtime_env) prog =
  let open Asm in
  let open Jit_env in
  let module JM = Jit_method in
  let reg, mem = Setup.env runtime_env `Meta_method prog in
  let { args } = Fundef.find_fuzzy prog "interp" in
  let trace_name = Trace_name.gen `Meta_method in
  let env =
    create_env
      ~trace_name:(Trace_name.value trace_name)
      ~red_names:(!Config.reds)
      ~index_pc:(List.index (Util.get_id "pc" args) args)
      ~merge_pc:pc
      ~bytecode:bytecode
  in
  let trace = JM.run prog reg mem env |> Jit_constfold.iter_fundef ~n:100 in
  Debug.with_debug (fun _ -> print_fundef trace);
  emit_and_compile prog `Meta_method trace


let jit_tracing ({bytecode; stack; pc; sp; bc_ptr; st_ptr} as runtime_env) prog =
  let open Asm in
  let open Jit_env in
  let module JT = Jit_tracing in
  let reg, mem = Setup.env runtime_env `Meta_tracing prog in
  let { args } = Fundef.find_fuzzy prog "interp" in
  let trace_name = Trace_name.gen `Meta_tracing in
  let env =
    create_env
      ~index_pc:(
        let pc_id = List.find (fun arg -> String.get_name arg = "pc") args in
        List.index pc_id args)
      ~merge_pc:pc
      ~trace_name:(Trace_name.value trace_name)
      ~red_names:(!Config.reds)
      ~bytecode:bytecode
  in
  let `Result (trace, others) = JT.run prog reg mem env in
  Debug.with_debug (fun _ -> print_fundef trace);
  if List.length others = 0 then
    emit_and_compile prog `Meta_tracing trace
  else
    emit_and_compile_with_so prog `Meta_tracing others trace

let exec_dyn_arg2 ~name ~arg1 ~arg2 =
  Dynload_stub.call_arg2
    ~lib:("./" ^ get_so_name name)
    ~func:(String.split_on_char '.' name |> List.hd)
    ~arg1:arg1 ~arg2:arg2

let exec_dyn_arg3 ~name ~arg1 ~arg2 ~arg3 =
  Dynload_stub.call_arg3
    ~lib:("./" ^ get_so_name name)
    ~func:(String.split_on_char '.' name |> List.hd)
    ~arg1:arg1 ~arg2:arg2 ~arg3:arg3

let jit_exec pc st_ptr sp stack =
  with_jit_flg ~off:(fun _ -> ()) ~on:begin fun _ ->
    match Trace_prof.find_opt pc with
    | Some (tname) ->
      (* Debug.print_stack stack; Printf.printf "[sp] %d\n" sp; *)
      Printf.eprintf "[tj] executing %s at pc: %d sp: %d ...\n" tname pc sp;
      let s = Unix.gettimeofday () in
      let _ = exec_dyn_arg2 ~name:tname ~arg1:st_ptr ~arg2:sp in
      let e = Unix.gettimeofday () in
      Printf.eprintf "[tj] ellapsed time: %f μ s\n" ((e -. s) *. 1e6);
      flush stdout;
      ()
    | None -> ()
  end

let jit_tracing_entry bytecode stack pc sp bc_ptr st_ptr =
  with_jit_flg ~off:(fun _ -> ()) ~on:begin fun _ ->
    if Trace_prof.over_threshold pc then
      begin match Trace_prof.find_opt pc with
        | Some _ -> ()
        | None ->
          let ic = file_open () in
          try
            let prog =
              ic |> Lexing.from_channel |> Opt.virtualize
              |> Jit_annot.annotate `Meta_tracing
            in
            close_in ic;
            let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
            match prog |> jit_tracing env with
            | Ok name -> Trace_prof.register (pc, name);
            | Error e -> raise e
          with e -> close_in ic; raise e
      end
    else
      Trace_prof.count_up pc
  end

let jit_method_call bytecode stack pc sp bc_ptr st_ptr =
  match Method_prof.find_opt pc with
  | Some name ->
    let s = Sys.time () in
    let r = exec_dyn_arg2 ~name:name ~arg1:st_ptr ~arg2:sp in
    let e = Sys.time () in
    Printf.eprintf "[mj] elapced time: %fus\n" ((e -. s) *. 1e6);
    flush stderr;
    r
  | None ->
    let ic = file_open () in
    try
      let p =
        ic |> Lexing.from_channel |> Opt.virtualize
        |> Jit_annot.annotate `Meta_method
      in
      close_in ic;
      let bytecode = Compat.of_bytecode bytecode in
      let env = { bytecode; stack; pc; sp; bc_ptr; st_ptr } in
      match p |> jit_method env with
      | Ok name ->
        Printf.eprintf "[mj] compiled %s at pc: %d\n" name pc;
        Method_prof.register (pc, name);
        let s = Sys.time () in
        let r = exec_dyn_arg2 ~name:name ~arg1:st_ptr ~arg2:sp in
        let e = Sys.time () in
        Printf.eprintf "[mj] elapced time: %F us\n" ((e -. s) *. 1e6);
        flush stderr;
        r
      | Error e -> raise e
    with e -> close_in ic; raise e


let method_compile bytecode stack pc sp bc_ptr st_ptr =
  let ic = file_open () in
  try
    let p =
      ic |> Lexing.from_channel |>  Opt.virtualize
      |> Jit_annot.annotate `Meta_method
    in
    close_in ic;
    let bytecode = Compat.of_bytecode bytecode in
    let env = {bytecode; stack; pc; sp; bc_ptr; st_ptr} in
    match jit_method env p with
    | Ok name ->
      Printf.eprintf "[mj] compiled %s at pc: %d\n" name pc;
      Method_prof.register (pc, name);
      flush stderr;
      ()
    | Error e ->
      ()
  with e ->
    close_in ic


let jit_method_compile_only bytecode stack pc sp bc_ptr st_ptr =
  Util.find_mj_entries bytecode |> List.map (fun pc_entry ->
      method_compile bytecode stack pc_entry sp bc_ptr st_ptr)


let callbacks () =
  Callback.register "jit_tracing_entry" jit_tracing_entry;
  Callback.register "jit_exec" jit_exec;
  Callback.register "jit_method_call" jit_method_call;
  Callback.register "jit_method_comp" jit_method_compile_only;
