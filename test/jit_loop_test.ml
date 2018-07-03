open OUnit
open MinCaml
open Asm
open Util
open Baccaml_jit
open Jit_config
open Jit_util

module MJ = Method_jit

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

let _ = run_test_tt_main begin
    "mj_loop_test" >::: [
      "test1" >::
      begin fun () ->
        Logger.log_level := Logger.Debug;
        let reg, mem = Array.make 1000000 (Red (-1)), Array.make 1000000 (Red (-1)) in
        (* execute preprocessor *)
        let fundefs', interp_body, jit_args' = Method_jit.prep' p "min_caml_test_trace" ["a"] in

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
        print_endline "[EXPERIMENT]"; List.iter (fun t ->
            print_endline "----------------------";
            Emit_virtual.to_string_fundef t |> print_endline;
            print_endline "----------------------"
          ) x;

        let y = Method_jit_loop.run_while p reg' mem' "min_caml_test_trace" ["bytecode"; "a"] in
        print_endline "[EXPERIMENT]"; List.iter (fun fundef ->
            print_endline "----------------------";
            Emit_virtual.to_string_fundef fundef |> print_endline;
            print_endline "----------------------"
          ) y;

        Jit_emit.emit_result_mj ~prog:p ~traces:y ~file:"jit_loop_test";
        ()
      end
    ]
  end
