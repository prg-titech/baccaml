open Base
open Asm

let trace_entry = "min_caml_trace_entry"

let ignore_list = ["min_caml_jit_dispatch"; "jit_merge_point"; "min_caml_jit_merge_point"]

let can_ignore id =
  ignore_list
  |> List.exists  (fun i -> i = id) ||
    (String.to_seq "trace" |> List.of_seq)
    |> List.exists (fun c -> String.contains id c)

let rec trim_global = function
  (* jit_dispatch (pc = 0) bytecode a *)
  | Let (_, Set (n0),
         Let (_, IfEq (x, y, Ans (Set (n1)), Ans (Set (n2))),
              Let (_, CallDir (Id.L (id), args, fargs),
                   body))) when (can_ignore id) ->
    begin
      Ans (
        IfEq (x, C (n0),
              Ans (CallDir (Id.L (trace_entry), List.tl args, fargs)),
              body))
    end
  | Let (_, IfEq (x, y, Ans (Set (n1)), Ans (Set (n2))),
         Let (_, CallDir (Id.L (id), args, fargs),
              body)) when (can_ignore id) ->
    Ans (
      IfEq (x, C (0),
            Ans (CallDir (Id.L (trace_entry), List.tl args, fargs)),
            body))
  | t -> t

let trim_jit_dispatcher t = trim_global t

let trim_jit_merge_point t = trim_global t

let trim t = trim_global t
