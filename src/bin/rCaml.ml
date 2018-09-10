open MinCaml
open Asm
open BacCaml
open Jit_config
open Jit_util

type env = {
  prog : prog;
  reg : value array;
  mem : value array;
  red_args : string list;
  ex_name : string;
} [@@deriving fields]

type arg = {
  file : string;
  ex_name : string;
  code : int array;
  annot : int array;
  reds : (string * int) list;
  greens : (string * int) list;
} [@@deriving fields]

type var = {
  redtbl : (string, int) Hashtbl.t;
  greentbl : (string, int) Hashtbl.t;
} [@@deriving fields]

type tenv = {
  fundefs : fundef list;
  ibody : Asm.t;
} [@@deriving fields]

let () =
  let r = { file = "test"; ex_name = "test"; code = [|0|]; annot = [|0|]; reds = []; greens = [] } in
  Fieldslib.(
    assert (file r = "test" && ex_name r = "test" && code r = [|0|] && annot r = [|0|] && reds r = [] && greens r = []);
    ignore (Fields_of_arg.create ~file:"a" ~ex_name:"b" ~code:(Array.make 1 0) ~annot:(Array.make 1 0) ~reds:[] ~greens:[]);
  )

let print_list f lst =
  let rec loop f = function
    | [] -> ()
    | hd :: tl -> f hd; print_string "; "; loop f tl
  in
  print_string "["; loop f lst; print_string "]"

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

let prepare_tenv' p t reds =
  let t' =
    Simm.t t
    |> Jit_trim.trim_jmp
    |> Jit_trim.trim_jit_dispatcher in
  begin match t' with
    | Let (_, Set (_),
           Let (_,  IfEq (_, _, _, _),
                Let (_, CallDir (Id.L (_), args, fargs),
                     interp_body)))
    | Let (_,  IfEq (_, _, _, _),
           Let (_, CallDir (Id.L (_), args, fargs),
                interp_body))
    | Ans (IfEq (_, _, Ans (CallDir (Id.L (_), args, fargs)),
                 interp_body)) ->
      let Prog (table, fundefs, main) = p in
      let fundefs' =
        List.map begin fun fundef ->
          let Id.L (x) = fundef.name in
          match String.split_on_char '.' x |> List.hd with
          | name' when name' = "interp" ->
            let { name; args; fargs; ret } = fundef in
            { name = name; args = args; fargs = fargs; body = interp_body; ret = ret }
          | _ -> fundef
        end fundefs
      in
      Fieldslib.(
        Fields_of_tenv.create ~fundefs:fundefs' ~ibody:interp_body
      )
    | _ ->
      failwith "missing jit_dispatch. please add jit_dispatch ... at the top of your interpreter."
  end

let prepare_tenv ~prog:p ~name:n ~red_args:reds =
  let Prog (table, fundefs, main) = p in
  let { body } = List.find begin fun { name = Id.L (x) } ->
      String.split_on_char '.' x |> List.hd |> contains "interp"
    end fundefs
  in
  prepare_tenv' p body (prepare_reds reds p)

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

let prepare_env arg =
  let p =
    open_in ((Sys.getcwd ()) ^ "/" ^ (Fieldslib.(file arg)))
    |> Lexing.from_channel
    |> Mutil.virtualize
    |> Simm.f
  in
  let reg, mem = Array.make 100000 (Red (0)), Array.make 100000 (Red (0)) in

  let red_args = List.map fst (Fieldslib.(reds arg)) in
  let tenv = prepare_tenv ~prog:p ~name:"min_caml_test_trace" ~red_args:red_args in

  (prepare_var (Fieldslib.(reds arg)) (Fieldslib.(greens arg))
   |> fun var ->
   Colorizer.colorize_reg
     (Fieldslib.(redtbl var))
     (Fieldslib.(greentbl var))
     reg
     (List.hd (Fieldslib.(fundefs tenv)))
     (Fieldslib.(ibody tenv));
   let addr =
     match Fieldslib.(greens arg) |> List.find_opt (fun arg' -> fst arg' = "bytecode") with
     | Some x -> x |> snd
     | None ->
       match Fieldslib.(reds arg) |> List.find_opt (fun arg' -> fst arg' = "bytecode") with
       | Some x -> x |> snd
       | None -> raise Not_found
   in
   prepare_prog (Fieldslib.(code arg)) addr (Fieldslib.(annot arg)) mem);

  Fieldslib.(
    Fields_of_env.create
      ~prog:p
      ~reg:reg
      ~mem:mem
      ~red_args:red_args
      ~ex_name:(Fieldslib.(ex_name arg)))

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

let file      = ref ""
let codes     = ref ""
let annots    = ref ""
let reds      = ref ""
let greens    = ref ""
let output    = ref "out"
let is_dryrun = ref false

let usage  = "usage: " ^ Sys.argv.(0) ^ " [-file string] [-green string list] [-red string list] [-code int list] [-annot int list]"

let speclist = [
  ("-file", Arg.Set_string file, "Specify file name");
  ("-green", Arg.Set_string greens, "Specify green variables");
  ("-red", Arg.Set_string reds, "Specify red variables");
  ("-code", Arg.Set_string codes, "Specify bytecode");
  ("-annot", Arg.Set_string annots, "Specify annotations for bytecode");
  ("-o", Arg.Set_string output, "Set executable's name");
  ("-dry-run", Arg.Unit (fun _ -> is_dryrun := true), "run as dry");
  ("-dbg", Arg.Unit (fun _ -> Logs.set_level @@ Some Logs.Debug), "Enable debug mode");
]

let run = (fun f ->
    Arg.parse
      speclist
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      usage;
    Logs.set_reporter @@ Logs.format_reporter ();
    let file = !file in
    let output = !output in
    let bytes = string_of_array ~f:int_of_string !codes in
    let annots = string_of_array ~f:int_of_string !annots in
    let reds = parse_pair_list !reds in
    let greens = parse_pair_list !greens in
    f Fieldslib.(
        Fields_of_arg.create
          ~file:file ~ex_name:output ~code:bytes ~annot:annots ~reds:reds ~greens:greens
      ))
