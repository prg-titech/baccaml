open Asm
open Core

let flg = ref false

let trace_entry = "min_caml_trace_entry"

let rec trim_jit_dispatcher = function
  (* jit_dispatch (pc = 0) bytecode a *)
  | Let (_, Set (n0),
         Let (_, IfEq (x, y, Ans (Set (n1)), Ans (Set (n2))),
              Let (_, CallDir (Id.L ("min_caml_jit_dispatch"), args, fargs),
                   body))) ->
    begin
      if !flg then
        Ans (
          IfEq (x, C (n0),
               Ans (CallDir (Id.L (trace_entry), List.tl_exn args, fargs)),
               body))
      else body
    end
  | Let (_, IfEq (x, y, Ans (Set (n1)), Ans (Set (n2))),
         Let (_, CallDir (Id.L ("min_caml_jit_dispatch"), args, fargs),
              body)) when !flg ->
    begin
      if !flg then
        Ans (
          IfEq (x, C (0),
                Ans (CallDir (Id.L (trace_entry), List.tl_exn args, fargs)),
                body))
      else body
    end
  | t -> t

let rec trim_jmp = function
  (* jit_dispatch (pc = 0) bytecode a *)
  | Let (_, Set (n0),
         Let (_, IfEq (x, y, Ans (Set (n1)), Ans (Set (n2))),
              Let (_, CallDir (Id.L ("jit_merge_point"), args, fargs),
                   body))) ->
    begin
      if !flg then
        Ans (
          IfEq (x, C (n0),
               Ans (CallDir (Id.L (trace_entry), List.tl_exn args, fargs)),
               body))
      else body
    end
  | Let (_, IfEq (x, y, Ans (Set (n1)), Ans (Set (n2))),
         Let (_, CallDir (Id.L ("jit_merge_point"), args, fargs),
              body)) when !flg ->
    begin
      if !flg then
        Ans (
          IfEq (x, C (0),
                Ans (CallDir (Id.L (trace_entry), List.tl_exn args, fargs)),
                body))
      else body
    end
  | t -> t


let f (Prog (table, fundefs, main)) =
  let fundefs' =
    List.map fundefs
      ~f:(fun { name; args; fargs; body; ret } ->
          let body' = trim_jmp body in
          { name = name
          ; args = args
          ; fargs = fargs
          ; body = body'
          ; ret = ret
          })
  in Prog (table, fundefs', main)
