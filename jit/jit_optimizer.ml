open Std
open MinCaml
open Asm
open Jit_env
open Jit_util

let run p e reg mem =
  match e with
  | Nop -> Specialized (Green 0)
  | Set n -> Specialized (Green n)
  | Mov id_t as exp ->
    let r = reg.(int_of_id_t id_t) in
    (match r with
    | Green n -> Specialized (Green n)
    | Red n -> Not_specialized (exp, Red n))
  | Neg id_t as exp ->
    let r = reg.(int_of_id_t id_t) in
    (match r with
    | Green n -> Specialized (Green n)
    | Red n -> Not_specialized (exp, Red n))
  | Add (id_t1, id_or_imm) as exp ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 =
      match id_or_imm with V id_t -> reg.(int_of_id_t id_t) | C n -> Green n
    in
    (match r1, r2 with
    | Green n1, Green n2 -> Specialized (Green (n1 + n2))
    | Red n1, Green n2 -> Not_specialized (Add (id_t1, C n2), Red (n1 + n2))
    | Green n1, Red n2 ->
      let id_t' =
        match id_or_imm with V id -> id | C n -> failwith "Add (green, red)"
      in
      Not_specialized (Add (id_t', C n1), Red (n1 + n2))
    | Red n1, Red n2 -> Not_specialized (exp, Red (n1 + n2)))
  | Sub (id_t1, id_or_imm) as exp ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 =
      match id_or_imm with V id_t -> reg.(int_of_id_t id_t) | C n -> Green n
    in
    (match r1, r2 with
    | Green n1, Green n2 -> Specialized (Green (n1 - n2))
    | Red n1, Green n2 -> Not_specialized (Sub (id_t1, C n2), Red (n1 - n2))
    | Green n1, Red n2 ->
      let id_t' =
        match id_or_imm with V id -> id | C n -> failwith "Sub (green, red)"
      in
      Not_specialized (Sub (id_t', C n1), Red (n1 - n2))
    | Red n1, Red n2 -> Not_specialized (exp, Red (n1 - n2)))
  | Mul (id_t1, id_or_imm) as exp ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 =
      match id_or_imm with V id_t -> reg.(int_of_id_t id_t) | C n -> Green n
    in
    (match r1, r2 with
    | Green n1, Green n2 -> Specialized (Green (n1 * n2))
    | Red n1, Green n2 -> Not_specialized (Mul (id_t1, C n2), Red (n1 * n2))
    | Green n1, Red n2 ->
      let id_t' =
        match id_or_imm with V id -> id | C n -> failwith "Sub (green, red)"
      in
      Not_specialized (Mul (id_t', C n1), Red (n1 * n2))
    | Red n1, Red n2 -> Not_specialized (exp, Red (n1 * n2)))
  | Ld (id_t, id_or_imm, x) as exp ->
    let destld = reg.(int_of_id_t id_t) in
    let offsetld =
      match id_or_imm with
      | V id_t ->
        (match reg.(int_of_id_t id_t) with
        | Green n1 -> Green (n1 * x)
        | Red n1 -> Red (n1 * x))
      | C n -> Green (n * x)
    in
    (match destld, offsetld with
    | Green n1, Green n2 ->
      (match mem.(n1 + n2) with
      | Green n as value -> Specialized value
      | Red n -> Not_specialized (Ld (zero, C (n1 + (n2 / x)), x), Red n))
    | Green n1, Red n2 ->
      let n = mem.(n1 + (n2 * x)) in
      reg.(int_of_id_t id_t) <- n;
      Not_specialized (Ld (id_t, id_or_imm, x), n)
    | Red n1, Green n2 ->
      (match mem.(n1 + n2) with
      | Green n -> Not_specialized (Ld (id_t, C (n1 + (n2 / x)), x), Red n)
      | Red n -> Not_specialized (Ld (id_t, C (n1 + (n2 / x)), x), Red n))
    | Red n1, Red n2 ->
      let n = mem.(n1 + n2) in
      Not_specialized (exp, Red (value_of n)))
  | St (src, dest, offset, x) ->
    let src' = reg.(int_of_id_t src) in
    let dest' = reg.(int_of_id_t dest) in
    let offset' =
      match offset with
      | V id_t ->
        (match reg.(int_of_id_t id_t) with
        | Green n -> Green n
        | Red n -> Red n)
      | C n -> Green n
    in
    (match dest', offset' with
    | Green n1, Green n2 ->
      (match src' with
      | Green n ->
        mem.(n1 + n2) <- src';
        Specialized (Green 0)
      | Red n -> Not_specialized (St (src, zero, C (n1 + n2), x), Red n))
    | Green n1, Red n2 -> failwith "St (_, green, red) isn't supported."
    | Red n1, Green n2 ->
      (match src' with
      | Green n ->
        mem.(n1 + n2) <- src';
        Not_specialized (St (src, dest, C (n1 + n2), x), Red 0)
      | Red n ->
        mem.(n1 + n2) <- src';
        (match offset' with
        | Green n -> Not_specialized (St (src, dest, C n, x), Red 0)
        | Red n -> Not_specialized (St (src, dest, offset, x), Red 0)))
    | Red n1, Red n2 ->
      (match src' with
      | Green n ->
        mem.(n1 + n2) <- Red (value_of src');
        Not_specialized (St (src, dest, C (n1 + n2), x), Red 0)
      | Red n ->
        (* [XXX]: PUT isn't worked well *)
        (* mem.(n1 + n2) <- src'; *)
        Not_specialized (St (src, dest, offset, x), Red 0)))
  | _ ->
    Asm.print_exp e;
    print_newline ();
    failwith "un suported instruction"
;;
