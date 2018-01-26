open Asm
open Core
open Jit_config

type rename_env = Id.t -> Id.t

let empty_env id =
  failwith (Format.sprintf "empty env %s" id)

let extend_env env id =
  let newid = Id.genid id in
  fun name ->
    if name = id then newid
    else env name

let rename_id_or_imm rename = function
  | V (id_t) -> V (rename id_t)
  | C (n) -> C (n)

let rec rename_exp rename = function
  | Nop -> Nop
  | Set (n) -> Set (n)
  | Mov (id_t) -> Mov (rename id_t)
  | Neg (id_t) -> Neg (rename id_t)
  | Add (id_t, id_or_imm) -> Add (rename id_t, rename_id_or_imm rename id_or_imm)
  | Sub (id_t, id_or_imm) -> Sub (rename id_t, rename_id_or_imm rename id_or_imm)
  | Ld (id_t, id_or_imm, x) -> Ld (rename id_t, rename_id_or_imm rename id_or_imm, x)
  | St (src, dest, id_or_imm, x) -> St (rename src, rename dest, rename_id_or_imm rename id_or_imm, x)
  | IfEq (id_t1, id_t2, t1, t2) -> IfEq (rename id_t1, rename_id_or_imm rename id_t2, rename_t rename t1, rename_t rename t2)
  | IfLE (id_t1, id_t2, t1, t2) -> IfLE (rename id_t1, rename_id_or_imm rename id_t2, rename_t rename t1, rename_t rename t2)
  | IfGE (id_t1, id_t2, t1, t2) -> IfGE (rename id_t1, rename_id_or_imm rename id_t2, rename_t rename t1, rename_t rename t2)
  | CallDir (id_l, args, fargs) -> CallDir (id_l, List.map ~f:rename args, List.map ~f:rename fargs)
  | exp -> exp

and rename_t env = function
  | Ans (exp) ->
    Ans (rename_exp env exp)
  | Let ((dest, typ), exp, body) ->
    let env' = extend_env env dest in
    Let ((env' dest, typ), rename_exp env' exp, rename_t env' body)

let rename_fundef ({name = name; args = args'; fargs = fargs; body = body; ret = ret;}) =
  let (args, rename) =
    List.fold_right
      ~f:(fun id (ids, env) -> let env' = extend_env env id in ((env' id) :: ids, env'))
      args'
      ~init:([], empty_env)
  in
  let res =
    { name = name
    ; args = args
    ; fargs = fargs
    ; body = rename_t rename body
    ; ret = ret
    }
  in res
