open Asm
open Util

exception Un_supported of string

type value = Red of int
           | Green of int

type jit_result = Specialized of value
                | Not_specialised of exp

let int_of_id_t = function (* TODO: レジスタ番号をsringで与える実装に変更 *)
  | "min_caml_hp" -> raise (Un_supported ("int_of_id_t min_caml_hp is not supported."))
  | id ->
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

let rec jitcompile (p : prog) (instr : t) (reg : value array) (mem : value array) : t =
  match instr with
  | Ans exp as e -> e
  | Let ((dest, typ), instr, body) ->
    (match jitcompile_instr p instr reg mem with
     | Specialized v ->
       reg.(int_of_id_t dest) <- v;
       jitcompile p body reg mem
     | Not_specialised e ->
       Let ((dest, typ), e, jitcompile p body reg mem))

and jitcompile_instr (p : prog) (e : exp) (reg : value array) (mem : value array) : jit_result =
  match e with
  | Set n ->
    Specialized (Green n)
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

