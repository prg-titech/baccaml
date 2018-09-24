open Core
open MinCaml
open BacCaml

let is_dump = ref `NG
let jit_type = ref `Not_specified

let tran_annot p =
  match !jit_type with
  | `Meta_tracing -> Jit_annot.trans_tj p
  | `Meta_method -> Jit_annot.trans_mj p
  | `Not_specified -> p


let dump file =
  In_channel.create file
  |> Lexing.from_channel
  |> Util.virtualize
  |> Simm.f
  |> tran_annot
  |> Emit_virtual.to_string_prog
  |> Out_channel.print_endline

let compile file =
  let f = match String.split file ~on:'.' |> List.hd with
    | Some v -> v
    | None -> file
  in
  let ic = In_channel.create (f ^ ".ml") in
  let oc = Out_channel.create (f ^ ".s") in
  try
    Lexing.from_channel ic
    |> Util.virtualize
    |> tran_annot
    |> Simm.f
    |> Trim.f
    |> RegAlloc.f
    |> Emit.f oc;
    In_channel.close ic;
    Out_channel.close oc
  with e ->
    In_channel.close ic;
    Out_channel.close oc;
    raise e

let specify_jit_type str =
  match str with
  | "tjit" -> jit_type := `Meta_tracing
  | "mjit" -> jit_type := `Meta_method
  | _ -> jit_type := `Not_specified

let usage =
  "usage: " ^ Sys.argv.(0) ^ " [-dump] [-type (tjit|mjit)]"

let spec_list = [
  ("-dump", Arg.Unit (fun _ -> is_dump := `OK), "Specify dump flag");
  ("-type", Arg.String specify_jit_type, "Specify jit type");
]

let _ =
  let files = ref [] in
  Arg.parse spec_list (fun s -> files := !files @ [s]) usage;
  List.iter !files ~f:(match !is_dump with `OK -> dump | `NG -> compile)
