open MinCaml
open Bc_lib
open Bc_jit
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
  ("-file", Arg.Set_string file, "Specify the filename of your interpreter");
  ("-annot", Arg.Set_string annots, "Specify annotations for your bytecodes");
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
    begin match jittype' with
      | `Meta_tracing -> Jit_tracing.(
          let tj_env = {
            trace_name = trace_name;
            red_args = red_args;
            index_pc = 3;
            merge_pc = merge_pc;
          } in
          let t = run prog reg mem tj_env in
          Logs.debug (fun m -> m "%s\n" (Emit_virtual.to_string_fundef t));
          [t]
        )
      | `Meta_method -> Jit_method.(
          let mj_env = {
            trace_name = trace_name;
            red_args = red_args;
          } in
          let t = run prog reg mem mj_env in
          ignore (t |> List.map (fun trace ->
              Logs.debug (fun m -> m "%s\n" (Emit_virtual.to_string_fundef trace))));
          t
        )
    end
    |> List.map Jit_elim.elim_fundef
    |> List.map Simm.h
    |> List.map RegAlloc.h
    |> Jit_emit_base.(
        let env = { jit_typ = jittype'; out = out; prog = prog; } in
        emit ~midflg:midflg env
      )
  with e ->
    close_in ic; raise e

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
