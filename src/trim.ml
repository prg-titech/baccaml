open Asm
open Core

let rec trim_jit_dispatcher = function
  (* jit_dispatch (pc = 0) bytecode a *)
  | Let (_, Set (_),
      Let (_,
         IfEq (x, y, _, _),
           Let (_, CallDir (Id.L ("min_caml_jit_dispatch"), args, fargs),
                body)))
  | Let (_,  IfEq (x, y, _, _),
         Let (_, CallDir (Id.L ("min_caml_jit_dispatch"), args, fargs),
              body)) ->
    Ans (
      IfEq (x, y,
            Ans (CallDir (Id.L ("min_caml_test_trace"), List.tl_exn args, fargs)),
            body))
  | Let (x, exp, body) -> Let (x, exp, trim_jit_dispatcher body)
  | t -> t
  

let f (Prog (table, fundefs, main)) =
  let fundefs' =
    List.map fundefs
      ~f:(fun { name; args; fargs; body; ret } ->
          let body' = trim_jit_dispatcher body in
          { name = name
          ; args = args
          ; fargs = fargs
          ; body = body'
          ; ret = ret
          })
  in Prog (table, fundefs', main)
