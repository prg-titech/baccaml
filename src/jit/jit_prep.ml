open MinCaml
open Asm
open Util
open Jit_util

type env = Env of Asm.fundef list * Asm.t * Id.t list

let create_mj_reds reds (Prog (_, fundefs, _)) =
  let interp =
    List.find begin fun { name = Id.L (x) } ->
      (String.split_on_char '.' x |> List.hd) = "interp"
    end fundefs in
  let { args } = interp in
  List.filter begin fun arg ->
    let arg_name = String.split_on_char '.' arg |> List.hd  in
    List.exists (fun s -> s = arg_name) reds
  end args

let prep' p t =
  let t' =
    Simm.t t
    |> Trim.trim_jmp
    |> Trim.trim_jit_dispatcher
  in
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
        Core.List.map fundefs ~f:begin fun fundef ->
            let Id.L (x) = fundef.name in
            match String.split_on_char '.' x |> List.hd with
            | name' when name' = "interp" ->
              let { name; args; fargs; ret } = fundef in
              { name = name; args = args; fargs = fargs; body = interp_body; ret = ret }
            | _ -> fundef
        end in
      fundefs', interp_body
    | _ ->
      failwith
        "missing jit_dispatch. please add jit_dispatch ... at the top of your interpreter."
  end

let prep ~prog:p ~name:n ~red_args:reds =
  let Prog (table, fundefs, main) = p in
  let { body } =
    List.find (fun { name = Id.L (x) } ->
      String.split_on_char '.' x
      |> List.hd
      |> contains "interp") fundefs in
  let fundefs', interp_body = prep' p body in
  Env (fundefs', interp_body, (create_mj_reds reds p))
