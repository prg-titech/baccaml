open Asm
open Core
open Jit_config
open Mincaml_util

exception No_function_defs of string

exception Tracing_jit_failed of string

module TJ = Tracing_jit

module MJ = Method_jit

let is_tracing = ref true

let is_method = ref false

let jit
    ~fname:f
    ~reds:reds
    ~greends:greens
    ~pc_place:pp
    ?(loop_header=None)
    ?(method_start=None)
    ?(method_end=None)
    ~inameo:inameo
    ~inamen:inamen =
  let reg = Array.create ~len:10000 (Red (0)) in
  let mem = Array.create ~len:10000 (Red (0)) in
  let inchan = In_channel.create (f ^ ".ml") in
  try
    Lexing.from_channel inchan
    |> virtualize
    |> Simm.f
    |> fun (Prog (_, fundefs, body) as prog) ->
    match List.hd fundefs with
    | Some { name; args; fargs; body; ret } ->
      if !is_tracing then
        let jit_args = {
          trace_name = "min_caml_test_trace";
          reds = reds;
          greens = greens;
          loop_header = Option.value loop_header ~default:(failwith "specify loop_header");
          loop_pc_place = pp;
        } in
        TJ.exec_tracing_jit prog body reg mem jit_args
        |> fun trace ->
        Jit_emit.emit_trace'
          ~fundef:trace
          ~fname:(f ^ "_tj")
          ~inameo:inameo
          ~inamen:inamen
      else if !is_method then
        let method_jit_args = {
          method_name = "min_caml_test_trace";
          reds = reds;
          method_start = Option.value method_start ~default:(failwith "specify method_start");
          method_end = Option.value method_end ~default:(failwith "specify method_end");
          pc_place = pp
        } in
        MJ.exec prog body reg mem method_jit_args
        |> fun trace ->
        Jit_emit.emit_trace'
          ~fundef:trace
          ~fname:(f ^ "_tj")
          ~inameo:inameo
          ~inamen:inamen
      else
        ()
    | None ->
      Printf.sprintf "No functions in %s" f
      |> No_function_defs
      |> fun e -> raise e
  with e ->
    In_channel.close inchan;
    Printf.sprintf "Executing tracing jit is failed in %s" f
    |> Tracing_jit_failed |> fun e -> raise e

let _ =
  let files = ref [] in
  Arg.parse
    [("--method-jit", Arg.Unit(fun _ -> is_method := true), "enable method jit");
     ("--tracing-jit", Arg.Unit(fun _ -> is_tracing := true), "enable tracing jit");]
    (fun s -> files := !files @ [s])
    ("BacCaml: an experimental meta-hybrid JIT compiler");
  List.iter
    ~f:(fun f -> ())
    !files

