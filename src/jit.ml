open Core

open Asm

exception Not_supported of string

module Util = struct
  type value =
    | Red of int
    | Green of int

  type jit_result =
    | Specialized of value
    | Not_specialised of exp

  type jit_branch_result =
    | Selected of t
    | Not_selected of exp

  type jit_args =
    { trace_name : string
    ; reds : string list
    ; greens: string list
    ; loop_header : int
    ; loop_pc : int
    }

  let enable_jit = ref false

  let value_of = function
    | Red (n) -> n
    | Green (n) -> n

  let is_red = function
    | Red _ -> true
    | Green _ -> false

  let is_green = function
    | Red _ -> false
    | Green _ -> true

  let int_of_id_t = function (* TODO: レジスタ番号をsringで与える実装に変更 *)
    | "min_caml_hp" -> raise (Not_supported ("int_of_id_t min_caml_hp is not supported."))
    | id ->
      match List.last (String.split id ~on:'.') with
      | Some v -> int_of_string v
      | None -> int_of_string id

  let rec get_body_by_id_l prog name =
    let Asm.Prog (_, fundefs, _) = prog in
    try
      List.find_exn fundefs ~f:(fun fundef -> fundef.name = name)
    with
    | _ -> failwith "Unknown"
end

open Util

module Inline = struct

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

  let rec inline_args argsr argst funbody =
    match argsr, argst with
    | [], [] ->
      funbody
    | hdr :: tlr, hdt :: tlt when hdr = hdt ->
      inline_args tlr tlt funbody
    | hdr :: tlr, hdt :: tlt ->
      Let ((hdt, Type.Int), Mov (hdr), (inline_args tlr tlt funbody))
    | _ ->
      failwith "Un matched pattern."

  let rec inline_calldir argsr argst dest funbody contbody =
    let rec add_cont_proc id_t instr =
      match instr with
      | Let (a, e, t) ->
        Let (a, e, add_cont_proc id_t t)
      | Ans e ->
        Let ((id_t, Type.Int), e, contbody)
    in
    add_cont_proc dest (inline_args argsr argst funbody)

  let rec inline_calldir_exp argsr fundef =
    let { args; body } = rename_fundef fundef in
    inline_args argsr args body

end

open Inline

module Guard = struct
  let select_branch e n1 n2 t1 t2 =
    match e with
    | IfEq _ ->
      if n1 = n2 then t1 else t2
    | IfLE _ ->
      if n1 <= n2 then t1 else t2
    | IfGE _ ->
      if n1 >= n2 then t1 else t2
    | _ ->
      failwith "Only IfEq, IfLE and IfGE should be come here."

  let rec get_free_vars = function
    | Ans (exp) -> get_free_vars' exp
    | Let ((dest, _), e, t) -> List.append (dest :: (get_free_vars' e)) (get_free_vars t)

  and get_free_vars' = function
    | Mov (id) -> [id]
    | Add (id_t, V (id)) | Sub (id_t, V (id)) ->  id_t :: id :: []
    | Add (id_t, C _) | Sub (id_t, C _) -> id_t :: []
    | Ld (dest, V (offset), _) -> dest :: offset :: []
    | Ld (dest, C (_), _) -> dest :: []
    | St (src, dest, V (offset), _) -> src :: dest :: offset :: []
    | St (src, dest, C (_), _) -> src :: dest :: []
    | IfEq (id_t1, V (id_t2), _, _) | IfLE (id_t1, V (id_t2), _, _) | IfGE (id_t1, V (id_t2), _, _) -> id_t1 :: id_t2 :: []
    | IfEq (id_t1, C (_), _, _) | IfLE (id_t1, C (_), _, _) | IfGE (id_t1, C (_), _, _) -> id_t1 :: []
    | CallDir (id_l, args, fargs) -> List.append args fargs
    | _ -> []

  let restore_green reg cont =
    let free_vars = get_free_vars cont in
    let rec restore cont = function
      | [] ->
        cont
      | hd :: tl ->
        match reg.(int_of_id_t hd) with
        | Green n ->
          Let ((hd, Type.Int), Set (n), restore cont tl)
        | Red _ ->
          restore cont tl
    in
    restore cont free_vars
end

open Guard

let is_first_enter = ref true

let rec jitcompile (p : prog) (instr : t) (reg : value array) (mem : value array) (jit_args : jit_args) : t =
  (* 毎回再帰呼び出しが行われたとき最初にもどったかどうかを調べる *)
  (* 戻ったらトレース最適化終了 *)
  (* CallDir を生成して終了 *)
  (* 新しい関数（トレース）の名前は外から決める、つまり引数にする *)
  (* その関数を CallDir する命令を生成すればいい *)
  (* 引数は red なレジスタ *)
  match instr with
  | Ans CallDir (id_l, argsr, argst') ->
    let fundef = get_body_by_id_l p id_l in
    (match !is_first_enter with
     | true when (value_of reg.(jit_args.loop_pc) = jit_args.loop_header) ->
       (* is_first_enter := false; *)
       jitcompile p (inline_calldir_exp argsr fundef) reg mem jit_args
     | false when (value_of reg.(jit_args.loop_pc) = jit_args.loop_header) ->
       let reds = List.filter ~f:(fun a -> is_red reg.(int_of_id_t a)) argsr in
       Ans (CallDir (Id.L (jit_args.trace_name), reds, []))
     | _ ->
       jitcompile p (inline_calldir_exp argsr fundef) reg mem jit_args
    )
  | Ans exp ->
    jitcompile_branch p exp reg mem jit_args
  | Let ((dest, typ), CallDir (id_l, argsr, argst), contbody) ->
    let fundef = get_body_by_id_l p id_l in
    let funbody = fundef.body in
    let argst = fundef.args in
    inline_calldir argsr argst dest funbody (jitcompile p contbody reg mem jit_args)
  | Let ((dest, typ), instr, body) ->
    (match jitcompile_instr p instr reg mem with
     | Specialized v ->
       reg.(int_of_id_t dest) <- v;
       jitcompile p body reg mem jit_args
     | Not_specialised e ->
       Let ((dest, typ), e, jitcompile p body reg mem jit_args)
    )

and jitcompile_branch (p : prog) (e : exp) (reg : value array) (mem : value array) (jit_args : jit_args) : t =
  match e with
  | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) | IfGE (id_t, id_or_imm, t1, t2) ->
    let r1 = reg.(int_of_id_t id_t) in
    let r2 = match id_or_imm with
      | V (id) -> reg.(int_of_id_t id)
      | C (n) -> Green (n)
    in
    (match r1, r2 with
     | Green (n1), Green (n2) ->
       jitcompile p (select_branch e n1 n2 t1 t2) reg mem jit_args
     | Green (n1), Red (n2) ->
       failwith "if (green, red)"
     | Red (n1), Green (n2) ->
       (match e with
        | IfEq _ ->
          if n1 = n2 then
            Ans (IfEq (id_t, C (n2), jitcompile p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfEq (id_t, C (n2), restore_green reg t1, jitcompile p t2 reg mem jit_args))
        | IfLE _ ->
          if n1 <= n2 then
            Ans (IfLE (id_t, C (n2), jitcompile p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfLE (id_t, C (n2), restore_green reg t1, jitcompile p t2 reg mem jit_args))
        | IfGE _ ->
          if n1 >= n2 then
            Ans (IfGE (id_t, C (n2), jitcompile p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfGE (id_t, C (n2), restore_green reg t1, jitcompile p t2 reg mem jit_args))
        | _ ->
          failwith "Not supported"
       )
     | _ ->
       let n1, n2 = value_of r1, value_of r2 in
       (match e with
        | IfEq _ ->
          if n1 = n2 then
            Ans (IfEq (id_t, id_or_imm, jitcompile p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfEq (id_t, id_or_imm, restore_green reg t1, jitcompile p t2 reg mem jit_args))
        | IfLE _ ->
          if n1 <= n2 then
            Ans (IfLE (id_t, id_or_imm, jitcompile p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfLE (id_t, id_or_imm, restore_green reg t1, jitcompile p t2 reg mem jit_args))
        | IfGE _ ->
          if n1 >= n2 then
            Ans (IfGE (id_t, id_or_imm, jitcompile p t1 reg mem jit_args, restore_green reg t2))
          else
            Ans (IfGE (id_t, id_or_imm, restore_green reg t1, jitcompile p t2 reg mem jit_args))
        | _ ->
          failwith "Not supported"
       ))
  | _ -> Ans e

and jitcompile_instr (p : prog) (e : exp) (reg : value array) (mem : value array) : jit_result =
  match e with
  | Set n ->
    Specialized (Green n)
  | Mov id_t as exp ->
    let r = reg.(int_of_id_t id_t) in
    (match r with
     | Green (n) ->
       Specialized (Green (n))
     | Red (n) ->
       Not_specialised (exp))
  | Add (id_t1, id_or_imm) as exp ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 = match id_or_imm with
      | V (id_t) -> reg.(int_of_id_t id_t)
      | C (n) -> Green (n)
    in
    (match r1, r2 with
     | Green (n1), Green (n2) ->
       Specialized (Green (n1 + n2))
     | Red (n1), Green (n2) ->
       Not_specialised (Add (id_t1, C (n2)))
     | Green (n1), Red (n2) ->
       failwith "Add (green, red)"
     | _ ->
       Not_specialised (exp))
  | Sub (id_t1, id_or_imm) as exp ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 = match id_or_imm with
      | V (id_t) -> reg.(int_of_id_t id_t)
      | C (n) -> Green (n)
    in
    (match r1, r2 with
     | Green (n1), Green (n2) ->
       Specialized (Green (n1 - n2))
     | Red (n1), Green (n2) ->
       (* green なものが残っていたら即値に置き換える *)
       Not_specialised (Sub (id_t1, C (n2)))
     | Green (n1), Red (n2) ->
       failwith "Sub (green, red)"
     | _ ->
       Not_specialised (exp))
  | Ld (id_t, id_or_imm, x) as exp ->
    let destld = reg.(int_of_id_t id_t) in
    let offsetld =
      (match id_or_imm with
       | V (id_t) ->
         (match reg.(int_of_id_t id_t) with
          | Green (n1) -> Green (n1 * x)
          | Red (n1) -> Red (n1 * x))
       | C (n) -> Green (n * x))
    in
    (match destld, offsetld with
     | Green (n1), Green (n2) ->
       (match mem.(n1 + n2) with
        | Green n as value ->
          Specialized (value)
        | Red n ->
          Not_specialised (exp))
     | Green (n1), Red (n2) ->
       failwith "Ld (green, red)"
     | Red (n1), Green (n2) ->
       Not_specialised (Ld (id_t, C (n2), x))
     | Red (n1), Red (n2) ->
       Not_specialised exp)
  | St (dest, src, offset, x) as exp ->
    let dest', src' = reg.(int_of_id_t dest), reg.(int_of_id_t src) in
    let offset' = match offset with
      | V (id_t) ->
        (match reg.(int_of_id_t id_t) with
         | Green (n) -> Green (n * x)
         | Red (n) -> Red (n * x))
      | C (n) -> Green (n * x)
    in
    (match src', offset' with
     | Green (n1), Green (n2) ->
       mem.(n1 + n2) <- dest';
       Specialized (Green (0))
     | Green (n1), Red (n2) ->
       failwith "St (green, red)"
     | Red (n1), Green (n2) ->
       Not_specialised (St (dest, src, C (n2), x))
     | _ ->
       Not_specialised (exp))
  | _ ->
    failwith "Not supported."

let exec_jitcompile p t reg mem jit_args =
  let res =
    jitcompile p t reg mem jit_args
  in
  { name = Id.L (jit_args.trace_name)
  ; args = jit_args.reds
  ; fargs = []
  ; body = res
  ; ret = Type.Int
  }
