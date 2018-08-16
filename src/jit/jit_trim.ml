open MinCaml
open Asm

open Core

let trace_entry = "min_caml_trace_entry"

let rec trim_jit_dispatcher = function
  (* jit_dispatch (pc = 0) bytecode a *)
  | Let (_, Set (n0),
         Let (_, IfEq (x, y, Ans (Set (n1)), Ans (Set (n2))),
              Let (_, CallDir (Id.L ("min_caml_jit_dispatch"), args, fargs),
                   body))) ->
    Ans (
      IfEq (x, C (n0),
            Ans (CallDir (Id.L (trace_entry), List.tl_exn args, fargs)),
            body))

  | Let (_, IfEq (x, y, Ans (Set (n1)), Ans (Set (n2))),
         Let (_, CallDir (Id.L ("min_caml_jit_dispatch"), args, fargs),
              body)) ->
    Ans (
      IfEq (x, C (0),
            Ans (CallDir (Id.L (trace_entry), List.tl_exn args, fargs)),
            body))
  | t -> t

let rec trim_jmp = function
  (* jit_dispatch (pc = 0) bytecode a *)
  | Let (_, Set (n0),
         Let (_, IfEq (x, y, Ans (Set (n1)), Ans (Set (n2))),
              Let (_, CallDir (Id.L ("jit_merge_point"), args, fargs),
                   body))) ->
    begin
      Ans (
        IfEq (x, C (n0),
              Ans (CallDir (Id.L (trace_entry), List.tl_exn args, fargs)),
              body))
    end
  | Let (_, IfEq (x, y, Ans (Set (n1)), Ans (Set (n2))),
         Let (_, CallDir (Id.L ("jit_merge_point"), args, fargs),
              body)) ->
    Ans (
      IfEq (x, C (0),
            Ans (CallDir (Id.L (trace_entry), List.tl_exn args, fargs)),
            body))
  | t -> t
