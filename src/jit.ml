open Core

open Asm
open JitUtil

exception Un_supported of string

module Inline = struct

  type rename_env = Id.t -> Id.t

  let id_env = ref [||]

  let rename id_t =
    match Array.find !id_env (fun (id, genned_id) -> id = id_t) with
    | Some (id, genned_id) -> genned_id
    | None ->
      let genned_id = Id.genid id_t in
      id_env := Array.append !id_env [|(id_t, genned_id)|];
      genned_id

  let rename_exp exp =
    match exp with
    | Nop -> Nop
    | Set (n) -> Set (n)
    | Mov (id_t) -> Mov (rename id_t)
    | Neg (id_t) -> Neg (rename id_t)
    | Add (id_t, id_or_imm) -> Add (rename id_t, id_or_imm)
    | Sub (id_t, id_or_imm) -> Sub (rename id_t, id_or_imm)
    | Ld (id_t, id_or_imm, x) -> Ld (rename id_t, id_or_imm, x)
    | St (src, dest, id_or_imm, x) -> St (src, rename dest, id_or_imm, x)
    | IfEq (id_t1, id_t2, t1, t2) -> IfEq (rename id_t1, id_t2, t1, t2)
    | IfLE (id_t1, id_t2, t1, t2) -> IfLE (rename id_t1, id_t2, t1, t2)
    | IfGE (id_t1, id_t2, t1, t2) -> IfGE (rename id_t1, id_t2, t1, t2)
    | _ -> exp

  let rec rename_t = function
    | Ans (exp) ->
      Ans (rename_exp exp)
    | Let ((dest, typ), exp, body) ->
      Let ((rename dest, typ), rename_exp exp, rename_t body)

  let rename_fundef ({name = name; args = args; fargs = fargs; body = body; ret = ret;}) =
    { name = name
    ; args = List.map args rename
    ; fargs = fargs
    ; body = rename_t body
    ; ret = ret }

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

  let rec inline_calldir_exp argsr argst funbody =
    inline_args argsr argst funbody
end

open Inline

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

let restore_green reg cont =
  let rec greens reg i acc =
    if i = (Array.length reg) then acc
    else
      (match reg.(i) with
       | Green (v) -> greens reg (i + 1) ((v, i) :: acc)
       | Red _ -> greens reg (i + 1) acc)
  in
  let greens = greens reg 0 [] in
  let rec restore greens cont =
    match greens with
    | [] -> cont
    | (v, n) :: tl -> Let (("Ti" ^ (string_of_int n) ^ "." ^ (string_of_int n), Type.Int), Set (v), restore tl cont)
  in
  restore greens cont

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
    let funbody = fundef.body in
    let argst = fundef.args in
    (match !is_first_enter with
     | true when (value_of reg.(jit_args.loop_pc) = jit_args.loop_header) ->
       is_first_enter := false;
       jitcompile p (inline_calldir_exp argsr argst funbody) reg mem jit_args
     | false when (value_of reg.(jit_args.loop_pc) = jit_args.loop_header) ->
       Ans (CallDir (Id.L (jit_args.trace_name), jit_args.reds, []))
     | _ ->
       jitcompile p (inline_calldir_exp argsr argst funbody) reg mem jit_args
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
       Let ((dest, typ), e, jitcompile p body reg mem jit_args))

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
     | _ ->
       let n1, n2 = value_of r1, value_of r2 in
       (match e with
        | IfEq _ ->
          if n1 = n2 then
            (* t1 の手前でレジスタの復元を行う *)
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
  } |> rename_fundef
