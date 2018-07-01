open OUnit
open Mincaml
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

let _ = run_test_tt_main begin
    "mj_loop_test" >::: [
      "test1" >::
      begin fun () ->
        Logger.log_level := Logger.Debug;
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
            7 |] in
        let p =
          open_in ((Sys.getcwd ()) ^ "/test/jit_loop.ml")
          |> Lexing.from_channel
          |> Mutil.virtualize
          |> Simm.f
        in
        let reg = Array.make 10000 (Red (-1)) in
        let mem = Array.make 10000 (Red (-1)) in

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
        
        (* execute function jit *)
        let res = match Method_jit.exec p (fundef'.body) reg mem () with
          | Method_success fundef | Tracing_success fundef -> fundef
        in
        print_endline "[RESULT]" |> fun () -> Emit_virtual.to_string_fundef res |> print_endline;

        (* extract loop function *)
        let loop = Mj_loop.find_loop "test_loop_fun" res.body in
        print_endline "[LOOP FUNCTION]" |> fun () -> Emit_virtual.to_string_fundef loop |> print_endline;
        
        (* extract non loop function *)
        let nonloop = Mj_loop.find_nonloop "test_loop_fun" res in
        print_endline "[NONLOOP FUNCTION]" |> fun () -> Emit_virtual.to_string_fundef nonloop |> print_endline;
        
        (* Jit_emit.emit_trace (Method_success (nonloop)) "nonloop" "interp.88";
         * Jit_emit.emit_trace (Method_success (loop)) "loop" "interp.88"; *)

        (* Jit_emit.emit_fundef loop |> Buffer.contents |> print_endline;
         * print_newline ();
         * Jit_emit.emit_fundef nonloop |> Buffer.contents |> print_endline; *)

        Jit_emit.emit_result_mj ~prog:p ~traces:([loop; nonloop]) ~file:"jit_loop_test";
        ()
      end;
    ]
  end
