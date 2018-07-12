open MinCaml
open Asm
open Util
open BacCaml
open Jit_config
open Jit_util

let print_list f lst =
  let rec loop f = function
    | [] -> ()
    | hd :: tl -> f hd; print_string "; "; loop f tl
  in
  print_string "["; loop f lst; print_string "]"

(* preprocesses *)
let bytecode =
  [|1; 1;
    7;
    1; 10;
    4;
    2; 13;
    6; 0;
    5;
    3; 5;
    7 |]

let p =
  open_in ((Sys.getcwd ()) ^ "/test/jit_loop.ml")
  |> Lexing.from_channel
  |> Mutil.virtualize
  |> Simm.f

let main =
  let reg, mem = Array.make 1000000 (Red (-1)), Array.make 1000000 (Red (-1)) in
  (* execute preprocessor *)
  let fundefs', interp_body, jit_args' =
    Method_jit_loop.prep ~prog:p ~name:"min_caml_test_trace" ~red_args:["a"] in

  let fundef' = List.hd fundefs' in
  let redtbl = Hashtbl.create 100 in
  let greentbl = Hashtbl.create 100 in

  Hashtbl.add greentbl "bytecode" 0;
  Hashtbl.add greentbl "pc" 3;
  Hashtbl.add redtbl "a" 100;
  Colorizer.colorize_reg redtbl greentbl reg fundef' interp_body;
  Colorizer.colorize_pgm bytecode 0 mem;

  let reg', mem' = Array.copy reg, Array.copy mem in
  let x = Method_jit_loop.run p reg mem "min_caml_test_trace" ["bytecode"; "a"] in
  Logger.debug "[EXPERIMENT]"; List.iter (fun t ->
      Logger.debug "----------------------";
      Emit_virtual.to_string_fundef t |> Logger.debug;
      Logger.debug "----------------------"
    ) x;

  let y = Method_jit_loop.run_while p reg' mem' "min_caml_test_trace" ["bytecode"; "a"] in
  Logger.debug "[EXPERIMENT]"; List.iter (fun fundef ->
      Logger.debug "----------------------";
      Emit_virtual.to_string_fundef fundef |> Logger.debug;
      Logger.debug "----------------------"
    ) y;

  Jit_emit.emit_result_mj ~prog:p ~traces:y ~file:"jit_loop_test";
  ()

let _ =
  (match Sys.argv with
  | [|"-debug"|] | [|"-d"|] | [|"--debug"|] ->
    Logger.log_level := Logger.Debug;
  | _ -> ());
  main
