open Asm
open Util

exception Un_supported of string

type value = Red of int
           | Green of int

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

let is_green id_t greens =
  List.mem (name_of_id_t id_t) greens

let rec jit
          (prog : prog)
          (instr : t)
          (reg : value array)
          (mem : value array) : t =
  match instr with
  | Let ((dest, typ), Add (id_t1, id_or_imm), body) ->
     let r1 = reg.(int_of_id_t id_t1) in
     let r2 = match id_or_imm with
       | V (id_t) -> reg.(int_of_id_t id_t)
       | C (n) -> Red (n)
     in
     (match r1, r2 with
      | Green (n1), Green (n2) ->
         let res = n1 + n2 in
         reg.(int_of_id_t dest) <- Green (res);
         jit prog body reg mem
      | _ ->
         Let ((dest, typ), Add ((id_t1), id_or_imm), jit prog body reg mem)
     )
  | Let ((dest, typ), Sub (id_t1, id_or_imm), body) ->
     let r1 = reg.(int_of_id_t id_t1) in
     let r2 = match id_or_imm with
       | V (id_t) -> reg.(int_of_id_t id_t)
       | C (n)-> Red (n)
     in
     (match r1, r2 with
      | Green (n1), Green (n2) ->
         let res = n1 - n2 in
         reg.(int_of_id_t dest) <- Green (res);
         jit prog body reg mem
      | _ ->
         Let ((dest, typ), Sub (id_t1, id_or_imm), jit prog body reg mem)
     )
  | Let ((dest, typ), Ld (id_t, id_or_imm, x), body) ->
     let destld = reg.(int_of_id_t id_t) in
     let offsetld = (match id_or_imm with
                     | V (id_t) ->
                        (match reg.(int_of_id_t id_t) with
                         | Green (n1) -> Green (n1 * x)
                         | Red (n1) -> Red (n1 * x)
                        )
                     | C (n) -> Red (n * x))
     in
     (match destld, offsetld with
      | Green (v1), Green (v2) ->
         let res = mem.(v1 + v2) in
         reg.(int_of_id_t dest) <- res;
         jit prog body reg mem
      | _ ->
         Let ((dest, typ), Ld (id_t, id_or_imm, x), jit prog body reg mem)
     )
  | Let ((dest, typ), op, body) ->
     Let ((dest, typ), op, jit prog body reg mem)
  | _ ->
     failwith "Not supported."

