open MinCaml
open Asm

exception Error of string

let find_interp_name fundefs =
  List.find begin fun { name = Id.L (x)} ->
    String.split_on_char '.' x
    |> List.hd
    |> fun s -> (String.equal "interp" s) || (String.equal "interpret" s)
  end fundefs |> fun { name = Id.L (x) } -> x

module Mid_layer = struct
  let emit oc jit_typ id_interp =
    Printf.fprintf oc ".globl min_caml_trace_entry\n";
    Printf.fprintf oc "min_caml_trace_entry:\n";
    Printf.fprintf oc "\tpushl\t%%eax\n";
    Printf.fprintf oc "\tpushl\t%%ecx\n";
    Printf.fprintf oc "\tcall\tmin_caml_test_trace\n";
    Printf.fprintf oc "\tpopl\t%%edx\n";
    Printf.fprintf oc "\tpopl\t%%edx\n";
    Printf.fprintf oc "\tret\n";
    Printf.fprintf oc ".globl min_caml_mid_layer\n";
    Printf.fprintf oc "min_caml_mid_layer:\n";
    (match jit_typ with
      `Meta_tracing ->
      Printf.fprintf oc "\tmovl\t%d(%%esp), %%eax\n" 8;
      Printf.fprintf oc "\tmovl\t%d(%%esp), %%ecx\n" 4;
    | `Meta_method ->
      Printf.fprintf oc "\tmovl\t%d(%%esp), %%eax\n" 12;
      Printf.fprintf oc "\tmovl\t%d(%%esp), %%ecx\n" 8);
    Printf.fprintf oc "\tjmp\t%s\n" id_interp;
end

type 'a env = {
  out : string;
  jit_typ : 'a;
  prog : prog;
  }

let emit_dynamic { out; jit_typ; prog = Prog (_, fundefs, _); } traces =
  let module E = Emit in
  let oc = open_out (out ^ ".s") in
  try
    let id_interp = find_interp_name fundefs in
    List.iter (fun trace -> E.h_cinterop oc trace) traces;
    close_out oc
  with e -> close_out oc; raise e

let emit ?(midflg=false) { out; jit_typ; prog = Prog (_, fundefs, _); } traces =
  Emit.(
    stackset := S.empty;
    stackmap := [];
    let oc = open_out (out ^ ".s") in
    try
      let id_interp = find_interp_name fundefs in
      List.iter (fun trace -> h oc trace) traces;
      if midflg then Mid_layer.emit oc jit_typ id_interp;
      close_out oc
    with e -> close_out oc; raise e
  )
