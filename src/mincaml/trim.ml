open Asm
open Core

let trace_entry = "min_caml_trace_entry"

let rec convert = function
  | Let (_, Set (n),
         Let (_, IfEq (x, y, Ans (_), Ans (_)),
              body)) ->
    begin match body with
      | Let ((dest, typ), CallDir (id_l, args, fargs), body)
        when (id_l = Id.L "min_caml_jit_dispatch" || id_l = Id.L "jit_merge_point") ->
        Ans (IfEq (x, C(n),
                   Ans (CallDir (Id.L ("min_caml_trace_entry"), List.tl_exn args, fargs)),
                   body
                  ))
      | t -> t
    end
  | t -> t

let f (Prog (table, fundefs, main)) =
  let fundefs' =
    List.map fundefs
      ~f:(fun { name; args; fargs; body; ret } ->
          let body' = body |> convert in
          { name = name
          ; args = args
          ; fargs = fargs
          ; body = body'
          ; ret = ret
          })
  in Prog (table, fundefs', main)
