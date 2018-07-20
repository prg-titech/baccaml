open MinCaml
open BacCaml
open RCaml
open Asm
open Util
open Jit_config
open Jit_util

let print_list f lst =
  let rec loop f = function
    | [] -> ()
    | hd :: tl -> f hd; print_string "; "; loop f tl
  in
  print_string "["; loop f lst; print_string "]"

let prepare_var red_lst green_lst =
  let red_tbl = Hashtbl.create 10 in
  let green_tbl = Hashtbl.create 10 in
  List.iter
    (fun r -> Hashtbl.add red_tbl (fst r) (snd r))
    red_lst;
  List.iter
    (fun g -> Hashtbl.add green_tbl (fst g) (snd g))
    green_lst;
  red_tbl, green_tbl

let main name ex_name code red_lst green_lst =
  let p =
    open_in ((Sys.getcwd ()) ^ "/" ^ name)
    |> Lexing.from_channel
    |> Mutil.virtualize
    |> Simm.f
  in
  let reg = Array.make 1000000 (Red (-1)) in
  let mem = Array.make 1000000 (Red (-1)) in

  let red_args = List.map fst red_lst in
  let tenv =
    RCaml.prepare_tenv ~prog:p ~name:"min_caml_test_trace" ~red_args:red_args in

  let fundef' = List.hd (Fieldslib.(fundefs tenv)) in

  let redtbl, greentbl = prepare_var red_lst green_lst in
  Colorizer.colorize_reg redtbl greentbl reg fundef' (Fieldslib.(ibody tenv));
  Colorizer.colorize_pgm code 0 mem;

  let y = Method_jit_loop.run_while p reg mem "min_caml_test_trace" ("bytecode" :: red_args) in
  List.iter (fun fundef -> Logs.debug (fun m ->
      m "%s" (Emit_virtual.to_string_fundef fundef);
    )) y;

  Jit_emit.emit_result_mj ~prog:p ~traces:y ~file:ex_name;
  ()

let to_tuple lst =
  if List.length (List.hd lst) <> 2 then
    failwith "to_tuple: element of list's size should be 2."
  else
    List.map (fun elm -> (List.nth elm 0, List.nth elm 1)) lst

let parse_string_list f str_lst =
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

let file = ref ""
let codes = ref ""
let reds = ref ""
let greens = ref ""
let output = ref "a.out"

let usage =  "usage: " ^ Sys.argv.(0) ^ " [-file string] [-greens string list] [-reds string list] [-codes int list]"

let speclist = [
  ("-file", Arg.Set_string file, "Specify file name");
  ("-green", Arg.Set_string greens, "Specify green variables");
  ("-red", Arg.Set_string reds, "Specify red variables");
  ("-code", Arg.Set_string codes, "Specify bytecode");
  ("-o", Arg.Set_string output, "Set executable's name");
  ("-dbg", Arg.Unit (fun _ -> Logs.set_level @@ Some Logs.Debug), "Enable debug mode");
]

let _ =
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  Logs.set_reporter @@ Logs.format_reporter ();
  let file = !file in
  let bytes = parse_string_list int_of_string !codes in
  let reds = parse_pair_list !reds in
  let greens = parse_pair_list !greens in
  let output = !output in
  main file output bytes reds greens
