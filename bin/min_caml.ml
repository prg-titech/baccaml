open MinCaml
open Jit

let output_file = ref None

let run_typ = ref `Emit

let jit_typ = ref `Not_specified

let need_interp_wo_hints = ref `No

let id x = x

let emit_interp_wo_hints p =
  let open Asm in
  let open Jit_elim_hints in
  match !need_interp_wo_hints with
  | `Yes ->
    (match !jit_typ with
    | `Not_specified -> p
    | (`Meta_tracing | `Meta_method) as typ ->
      let Prog (flttbl, strtbl, fundefs, main) = p in
      Prog (flttbl, strtbl, elim_hints_and_rename typ fundefs, main))
  | `No -> p

let virtualize l = Opt.virtualize l |> emit_interp_wo_hints

let open_out_file f =
  match !output_file with
  | Some name -> open_out @@ Filename.remove_extension name ^ ".s"
  | None -> open_out @@ Filename.remove_extension f ^ ".s"

let annot p =
  match !jit_typ with
  | `Not_specified -> p
  | (`Meta_method | `Meta_tracing) as typ -> Jit_annot.annotate typ p

let run_dump f =
  let inchan = open_in f in
  try
    Lexing.from_channel inchan |> virtualize |> Simm.f |> annot
    |> Asm.print_prog;
    close_in inchan
  with e -> close_in inchan ; raise e

let run_interp f =
  let ic = open_in f in
  try
    Lexing.from_channel ic |> virtualize |> Simm.f |> annot |> Interp.f
    |> string_of_int |> print_endline
  with e -> close_in ic ; raise e

let run_compile f =
  let inchan = open_in f in
  let outchan = open_out_file f in
  try
    Lexing.from_channel inchan
    |> virtualize
    |> Simm.f
    |> annot
    |> RegAlloc.f |> Emit.f outchan ;
    close_in inchan ;
    close_out outchan
  with e -> close_in inchan ; close_out outchan ; raise e

let print_ast f =
  let ic = open_in f in
  Id.counter := 0;
  Typing.extenv := M.empty;
  try
    Lexing.from_channel ic
    |> Parser.exp Lexer.token
    |> Syntax.show |> print_endline;
    close_in ic
  with e -> close_in ic; raise e

let spec_list =
  let set_jittyp str = match str with
    | "mjit" -> jit_typ := `Meta_method
    | "tjit" -> jit_typ := `Meta_tracing
    | _ -> ()
  in
  [ ("-o", Arg.String (fun out -> output_file := Some out), "output file")
  ; ( "-inline", Arg.Int (fun i -> Inline.threshold := i), "maximum size of functions inlined" )
  ; ( "-iter", Arg.Int (fun i -> Opt.limit := i), "maximum number of optimizations iterated" )
  ; ( "-type" , Arg.String (set_jittyp), "specify jit type" )
  ; ("-err", Arg.Unit (fun _ -> Log.log_level := `Error), "Specify loglevel as error")
  ; ("-debug", Arg.Unit (fun _ -> Log.log_level := `Debug), "Specify loglevel as debug")
  ; ("-dump", Arg.Unit (fun _ -> run_typ := `Dump), "emit virtual machine code")
  ; ("-ast", Arg.Unit (fun _ -> run_typ := `Ast), "emit ast")
  ; ("-no-hint", Arg.Unit (fun _ -> need_interp_wo_hints := `Yes), "eliminate hint functions written in your meta-interp.")
  ; ("-interp", Arg.Unit (fun _ -> run_typ := `Interp), "run as interpreter") ]

let usage =
  "Mitou Min-Caml Compiler (C) Eijiro Sumii\n"
  ^ Printf.sprintf "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..."
      Sys.argv.(0)

let () =
  let files = ref [] in
  Arg.parse spec_list (fun f -> files := !files @ [f]) usage ;
  !files
  |> List.iter
       (match !run_typ with
        | `Ast -> print_ast
        | `Dump -> run_dump
        | `Interp -> run_interp
        | `Emit -> run_compile)
