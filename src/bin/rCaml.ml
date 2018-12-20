open MinCaml
open Bc_front_lib
open Asm
open BacCaml
open Jit_util

type env = {
  prog : prog;
  reg : value array;
  mem : value array;
  red_args : string list;
  ex_name : string;
  merge_pc : int;
  trace_name : string;
} [@@deriving fields]

type arg = {
  file : string;
  ex_name : string;
  code : int array;
  annot : int array;
  reds : (string * int) list;
  greens : (string * int) list;
  merge_pc: int;
  trace_name : string;
} [@@deriving fields]

type var = {
  redtbl : (string, int) Hashtbl.t;
  greentbl : (string, int) Hashtbl.t;
} [@@deriving fields]

type tenv = {
  fundefs : fundef list;
  ibody : Asm.t;
} [@@deriving fields]

exception Error of string

let prepare_reds trace_args (Prog (_, fundefs, _)) =
  let interp =
    List.find begin fun { name = Id.L (x) } ->
      (String.split_on_char '.' x |> List.hd) = "interp"
    end fundefs
  in
  let { args } = interp in
  List.filter begin fun arg ->
    let arg_name = List.hd (String.split_on_char '.' arg) in
    List.exists (fun trace_arg -> trace_arg = arg_name) trace_args
  end args

let prepare_tenv ~prog:p ~name:n ~red_args:reds =
  let Prog (table, fundefs, main) = p in
  let { body } =
    List.find begin fun { name = Id.L (x) } ->
      String.split_on_char '.' x |> List.hd |> contains "interp"
    end fundefs
  in
  begin match Simm.t body |> Jit_trim.trim with
    | Let (_, Set (_), Let (_,  IfEq (_, _, _, _), Let (_, CallDir (id_l, args, fargs), interp_body)))
    | Let (_,  IfEq (_, _, _, _), Let (_, CallDir (id_l, args, fargs), interp_body))
    | Ans (IfEq (_, _, Ans (CallDir (id_l, args, fargs)), interp_body)) ->
      { fundefs =
          List.map begin fun fundef ->
            let Id.L (x) = fundef.name in
            match String.split_on_char '.' x |> List.hd with
            | name' when name' = "interp" ->
              let { name; args; fargs; ret } = fundef in
              { name = name; args = args; fargs = fargs; body = interp_body; ret = ret }
            | _ -> fundef
          end fundefs;
        ibody = interp_body; }
    | _ ->
      raise (Error "Missing hint function: jit_dispatch")
  end

let prepare_var red_lst green_lst =
  let red_tbl = Hashtbl.create 10 in
  let green_tbl = Hashtbl.create 10 in
  List.iter
    (fun r -> Hashtbl.add red_tbl (fst r) (snd r))
    red_lst;
  List.iter
    (fun g -> Hashtbl.add green_tbl (fst g) (snd g))
    green_lst;
  { redtbl = red_tbl; greentbl = green_tbl }

let prepare_prog bytecode addr annot mem =
  for i = 0 to (Array.length bytecode - 1) do
    if Array.exists (fun annot -> annot = i) annot then
      mem.(addr + i * 4) <- Red (bytecode.(i))
    else
      mem.(addr + i * 4) <- Green (bytecode.(i))
  done

let prepare_env jit_type { file; ex_name; code; annot; reds; greens; merge_pc; trace_name } =
  let p =
    open_in file
    |> Lexing.from_channel
    |> Util.virtualize
    |> Simm.f
    |> Jit_annot.gen_mj jit_type
  in
  let reg = Array.make 10000000 (Red (0)) in
  let mem = Array.make 10000000 (Red (0)) in
  let red_args = List.map fst reds in
  let tenv =
    prepare_tenv
      ~prog:p
      ~name:trace_name
      ~red_args:red_args in
  let { greentbl; redtbl } = prepare_var reds greens  in
  Colorizer.colorize_reg redtbl greentbl reg (List.hd (tenv.fundefs)) (tenv.ibody);
  let addr =
    match
      greens |> List.find_opt (fun arg' -> fst arg' = "bytecode" || fst arg' = "code")
    with
     | Some x -> x |> snd
     | None ->
       match
         reds |> List.find_opt (fun arg' -> fst arg' = "bytecode" || fst arg' = "code")
       with
       | Some x -> x |> snd
       | None -> raise Not_found
   in
   prepare_prog code addr annot mem;
   { prog = p;
     reg = reg;
     mem = mem;
     red_args = red_args;
     ex_name = ex_name;
     merge_pc = merge_pc;
     trace_name = trace_name }

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

exception Jittype_error of string

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

let run = (fun f ->
    let prog_file_name = ref "" in
    Arg.parse
      speclist
      (fun f -> prog_file_name := f)
      usage;
    Logs.set_reporter @@ Logs.format_reporter ();
    let file = !file in
    let output = !output in
    (* let bytes = Util.string_of_array ~f:int_of_string !codes in *)
    let bytes = Bc_front_lib.Bc_front.(parse_stdin |> array_of_exps) in
    let annots = Util.string_of_array ~f:int_of_string !annots in
    let reds = Util.parse_pair_list !reds in
    let greens = Util.parse_pair_list !greens in
    let jittype' = match !jittype with
      | "tjit" -> `Meta_tracing
      | "mjit" -> `Meta_method
      | _ -> raise @@ Jittype_error "-type (tjit|mjit) is missing."
    in
    let arg = { file = file;
                ex_name = output;
                code = bytes;
                annot = annots;
                reds = reds;
                greens = greens;
                merge_pc = !merge_pc;
                trace_name = !name } in
    f jittype' (prepare_env jittype' arg))
