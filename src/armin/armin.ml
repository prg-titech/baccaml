open Core
open MyFront
open Syntax
open Virtual
open Compiler

let main f =
  let ic = In_channel.create f in
  try Armin_impl.entry ic with
    e -> In_channel.close ic

let _ =
  Logs.set_level @@ Some Logs.Info;
  Logs.set_reporter @@ Logs.format_reporter ();
  let files = ref [] in
  Arg.parse
    [("-debug", Arg.Unit (fun _ -> (Logs.set_level @@ Some Logs.Debug)), "set loglevel as debug.");
     ("-err", Arg.Unit (fun _ -> (Logs.set_level @@ Some Logs.Error)), "set loglevel as error.");]
    (fun f -> files := !files @ [f]) "The interface language for BacCaml.";
  List.iter ~f:(fun f -> main f) !files
