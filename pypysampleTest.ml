open OUnit
open Asm
open Jit

let dir = "min-interp/"

let rec iter n e = (* 最適化処理をくりかえす (caml2html: main_iter) *)
  Format.eprintf "iteration %d@." n;
  if n = 0 then e else
    let e' = Elim.f (ConstFold.f (Inline.f (Assoc.f (Beta.f e)))) in
    if e = e' then e else
      iter (n - 1) e'

let virtualize l =
  Id.counter := 0;
  Typing.extenv := M.empty;
  Parser.exp Lexer.token l
  |> Typing.f
  |> KNormal.f
  |> iter 1000
  |> Alpha.f
  |> Closure.f
  |> Virtual.f

let _ = run_test_tt_main begin
    "tracing jit test" >::: [
      "test 1" >::
      begin fun () ->
        Logger.log_level := Logger.Debug;
        let ic = open_in (dir ^ "pypysample.ml") in
        let lexbuf = Lexing.from_channel ic in
        let prog = virtualize lexbuf in
        let Prog (_, [fundef], _) = prog in
        let instr = fundef.body in
        let reg = Array.make 1000 (Red 0) in
        let mem = Array.make 1000 (Red 0) in
        reg.(107) <- Green (0);
        reg.(108) <- Green (4);
        reg.(109) <- Red (10);
        reg.(110) <- Red (100 * 4);
        mem.(0 * 4) <- Green (1);
        mem.(1 * 4) <- Green (0);
        mem.(2 * 4) <- Green (1);
        mem.(3 * 4) <- Green (2);
        mem.(4 * 4) <- Green (2);
        mem.(5 * 4) <- Green (0);
        mem.(6 * 4) <- Green (4);
        mem.(7 * 4) <- Green (1);
        mem.(8 * 4) <- Green (0);
        mem.(9 * 4) <- Green (2);
        mem.(10 * 4) <- Green (2);
        mem.(11 * 4) <- Green (3);
        mem.(12 * 4) <- Green (1);
        mem.(13 * 4) <- Green (1);
        mem.(14 * 4) <- Green (2);
        mem.(15 * 4) <- Green (2);
        mem.(16 * 4) <- Green (0);
        mem.(17 * 4) <- Green (0);
        mem.(18 * 4) <- Green (4);
        mem.(19 * 4) <- Green (2);
        mem.(20 * 4) <- Green (2);
        mem.(21 * 4) <- Green (5);
        mem.(100 * 4) <- Red (10);
        let trace = exec_jitcompile prog instr reg mem in
        print_string (EmitVirtual.to_string_prog trace);
        ()
      end
    ]
  end
