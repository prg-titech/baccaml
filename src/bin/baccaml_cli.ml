open MinCaml
open RCaml
open BacCaml
open Bc_front_lib

exception Jittype_error of string

module Util = struct
  let to_tuple lst =
    if List.length lst = 0 then
      [("dummy", "0")]
    else if List.length (List.hd lst) <> 2 then
      failwith "to_tuple: element of list's size should be 2."
    else
      List.map (fun elm -> (List.nth elm 0, List.nth elm 1)) lst

  let string_of_array ~f str_lst =
    str_lst
    |> Str.split_delim (Str.regexp " ")
    |> List.map f
    |> Array.of_list

  (* parse a list like "a 1; b 2" -> [("a", 1), ("b", 2)] *)
  let parse_pair_list pair_lst =
    pair_lst
    |> Str.split_delim (Str.regexp "; ")
    |> List.map (Str.split_delim (Str.regexp " "))
    |> to_tuple
    |> List.map (fun (x, y) -> (x, int_of_string y))

  let print_list f lst =
    let rec loop f = function
      | [] -> ()
      | hd :: tl -> f hd; print_string "; "; loop f tl
    in
    print_string "["; loop f lst; print_string "]"
end

let file      = ref ""
let codes     = ref ""
let annots    = ref ""
let reds      = ref ""
let greens    = ref ""
let output    = ref "out"
let jittype   = ref ""
let merge_pc  = ref 0
let name      = ref "min_caml_test_trace"

let usage  = "usage: " ^ Sys.argv.(0) ^ " [-file string] [-green string list] [-red string list] [-code int list] [-annot int list]"

let speclist = [
  ("-file", Arg.Set_string file, "Specify file name");
  ("-green", Arg.Set_string greens, "Specify green variables");
  ("-red", Arg.Set_string reds, "Specify red variables");
  ("-code", Arg.Set_string codes, "Specify bytecode");
  ("-annot", Arg.Set_string annots, "Specify annotations for bytecode");
  ("-type", Arg.Set_string jittype, "Specify jit type");
  ("-merge-pc", Arg.Set_int merge_pc, "Specify merge pc");
  ("-name", Arg.Set_string name, "Specify the name of trace");
  ("-o", Arg.Set_string output, "Set executable's name");
  ("-dbg", Arg.Unit (fun _ -> Logs.set_level @@ Some Logs.Debug), "Enable debug mode");
]

let run
    ?midflg:(midflg=false)
    ?out:(out="a.out")
    ?tr:(tr="min_caml_test_trace")
    f =
  let file = !file in
  (* let bytes = Util.string_of_array ~f:int_of_string !codes in *)
  (* let bytes = Bc_front.(parse_stdin |> array_of_exps) in *)
  let ic = open_in f in
  try
    let bc_front_env = Bc_front.(env_from_channel ic) in
    let annots = Util.string_of_array ~f:int_of_string !annots in
    let jittype' = match bc_front_env.jit_type with
      | "tjit" -> `Meta_tracing
      | "mjit" -> `Meta_method
      | _ -> raise @@ Jittype_error "-type (tjit|mjit) is missing."
    in
    let { prog; reg; mem; red_args; ex_name; merge_pc; trace_name } = {
      file = file;
      ex_name = out;
      trace_name = tr;
      code = bc_front_env.insts;
      annot = annots;
      reds = bc_front_env.red;
      greens = bc_front_env.green;
      merge_pc = bc_front_env.merge_pc;
    } |> prepare_env jittype' in
    let trace = match jittype' with
      | `Meta_tracing ->
        [Jit_tracing.run_while prog reg mem trace_name red_args 3 merge_pc]
      | `Meta_method ->
        Jit_method.run_while prog reg mem trace_name red_args
    in
    Logs.debug (fun m ->
        trace |> List.iter (fun t ->
            m "%s\n" (Emit_virtual.to_string_fundef t)));
    Jit_emit.emit_result
      ~midflg:midflg
      ~out:out
      ~jit_type:jittype'
      ~prog:prog
      ~traces:trace;
    close_in ic;
  with e ->
    close_in ic;
    raise e

let () =
  let files = ref [] in
  Arg.parse
    speclist
    (fun f -> files := !files @ [f])
    usage;
  Logs.set_reporter @@ Logs.format_reporter ();
  let tr = "min_caml_test_trace" in
  !files |> List.iteri (fun i f ->
      let out = f |> Filename.basename |> Filename.remove_extension in
      if i = 0 then run ~midflg:true ~tr:tr ~out:out f
      else run ~tr:(tr ^ "_" ^ (string_of_int i)) ~out:out f
    )
