open Asm
open Util

exception Un_supported of string

type value =
  | Red of int
  | Green of int

type jit_result =
  | Specialized of value
  | Not_specialised of exp

type jit_branch_result =
  | Selected of t
  | Not_selected of exp

let int_of_id_t id =
  try
    int_of_string (String.after_of id '.')
  with _ ->
    int_of_string (String.after_of id 'u')

let int_of_id_or_imm = function
  | V (id_t) -> int_of_id_t id_t
  | C (n) -> n

let value_of = function
  | Red (n) -> n
  | Green (n) -> n

let name_of_id_t str =
  List.hd (Str.split (Str.regexp_string ".") str)

let rec get_body_by_id_l prog name =
  let Prog (_, fundefs, _) = prog in
  List.find (fun fundef -> fundef.name = name) fundefs

let rec unroll argsr argst dest funbody contbody =
  let rec loop argsr argst =
    match argsr, argst with
    | [], [] ->
      funbody
    | hdr :: tlr, hdt :: tlt ->
       Let ((hdt, Type.Int), Mov (hdr), (loop tlr tlt))
    | _ ->
       failwith "Un matched pattern."
  in
  let rec add_cont_proc id_t instr =
    match instr with
    | Let (a, e, t) -> Let (a, e, add_cont_proc id_t t)
    | Ans e -> Let ((id_t, Type.Int), e, contbody)
  in
  add_cont_proc dest (loop argsr argst)

let rec jitcompile (p : prog) (instr : t) (reg : value array) (mem : value array) : t =
  match instr with
  | Ans exp ->
     (match jitcompile_branch p exp reg mem with
      | Selected (instr) -> jitcompile p instr reg mem
      | Not_selected (x) -> Ans (x))
  | Let ((dest, typ), CallDir (id_l, argsr, _), contbody) ->
     let fundef = get_body_by_id_l p id_l in
     let funbody = fundef.body in
     let argst = fundef.args in
     unroll argsr argst dest funbody (jitcompile p contbody reg mem)
  | Let ((dest, typ), instr, body) ->
    (match jitcompile_instr p instr reg mem with
     | Specialized v ->
       reg.(int_of_id_t dest) <- v;
       jitcompile p body reg mem
     | Not_specialised e ->
       Let ((dest, typ), e, jitcompile p body reg mem))

and jitcompile_branch (p : prog) (e : exp) (reg : value array) (mem : value array) : jit_branch_result =
  match e with
  | IfEq (id_t, id_or_imm, t1, t2) | IfLE (id_t, id_or_imm, t1, t2) | IfGE (id_t, id_or_imm, t1, t2) ->
     let r1 = reg.(int_of_id_t id_t) in
     let r2 = match id_or_imm with
       | V (id) -> reg.(int_of_id_t id)
       | C (n) -> Green (n)
     in
     (match r1, r2 with
      | Green (n1), Green (n2) ->
         (match e with
          | IfEq _ ->
             if n1 = n2 then Selected (t1)
             else Selected (t2)
          | IfLE _ ->
             if n1 <= n2 then Selected (t1)
             else Selected (t2)
          | IfGE _ ->
             if n1 >= n2 then Selected (t1)
             else Selected (t2)
          | _ ->
             failwith "Only IfEq, IfLE and IfGE should be come here.")
      | _ -> Not_selected (e))
  | _ -> Not_selected (e)

and jitcompile_instr (p : prog) (e : exp) (reg : value array) (mem : value array) : jit_result =
  match e with
  | Set n ->
    Specialized (Green n)
  | Mov id_t as exp ->
    let r = reg.(int_of_id_t id_t) in
    (match r with
     | Green (n) -> Specialized (Green (n))
     | Red (n) -> Not_specialised (exp))
  | Add (id_t1, id_or_imm) as exp ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 = match id_or_imm with
      | V (id_t) ->
        reg.(int_of_id_t id_t)
      | C (n) ->
        (match r1 with
         | Green _ -> Green (n)
         | Red _ -> Red (n))
    in
    (match r1, r2 with
     | Green (n1), Green (n2) ->
       Specialized (Green (n1 + n2))
     | _ ->
       Not_specialised (exp))
  | Sub (id_t1, id_or_imm) as exp ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 = match id_or_imm with
      | V (id_t) ->
        reg.(int_of_id_t id_t)
      | C (n) ->
        (match r1 with
         | Green _ -> Green n
         | Red _ -> Red n)
    in
    (match r1, r2 with
     | Green (n1), Green (n2) ->
       Specialized (Green (n1 - n2))
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
       | C (n) -> Red (n * x))
    in
    (match destld, offsetld with
     | Green (n1), Green (n2) ->
       (match mem.(n1 + n2) with
        | Green n as value -> Specialized (value)
        | Red n -> Not_specialised (exp)
       )
     | _ ->
       Not_specialised exp)
  | St (dest, src, offset, x) as exp ->
    let dest', src' = reg.(int_of_id_t dest), reg.(int_of_id_t src) in
    let offset' = match offset with
      | V (id_t) ->
        reg.(int_of_id_t id_t)
      | C (n) ->
        (match dest', src' with
         | Green _, Green _ -> Green (n * x)
         | _ -> Red (n * x))
    in
    (match dest', src', offset' with
     | Green (v1), Green (v2), Green (v3) ->
       mem.(v2 + v3) <- Green (v1);
       Specialized (Green (0))
     | _ ->
       Not_specialised (exp))
  | _ ->
    failwith "Not supported."
