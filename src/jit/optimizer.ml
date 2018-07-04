open Core

open MinCaml
open Util
open Asm
open Jit_config
open Jit_util

exception Not_optimization_supported of string

let run p e reg mem = match e with
  | Nop -> Specialized (Red 0)
  | Set n ->
    Specialized (Green n)
  | Mov id_t as exp ->
    let r = reg.(int_of_id_t id_t) in
    (match r with
     | Green (n) ->
       let msg = Format.sprintf "Set (%d): Green" n in
       Logger.debug msg;
       Specialized (Green (n))
     | LightGreen (n) ->
       let msg = Format.sprintf "Set (%d): LightGreen" n in
       Logger.debug msg;
       Specialized (LightGreen (n))
     | Red (n) ->
       let msg = Format.sprintf "Set (%d): Red" n in
       Logger.debug msg;
       Not_specialized (exp, Red (n)))
  | Add (id_t1, id_or_imm) as exp ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 = match id_or_imm with
      | V (id_t) -> reg.(int_of_id_t id_t)
      | C (n) -> Green (n)
    in
    let id_t2 = match id_or_imm with V (id) -> id | C (n) -> string_of_int n in
    (match r1, r2 with
     | Green (n1), Green (n2)
     | LightGreen (n1), LightGreen (n2)
     | LightGreen (n1), Green (n2)
     | Green (n1), LightGreen (n2) ->
       let msg =
         Format.sprintf "Add (%s, %s), %d %d ==> %d: Green, Green"
           id_t1 id_t2 (value_of r1) (value_of r2) (n1 + n2)
       in Logger.debug msg;
       Specialized (Green (n1 + n2))
     | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
       let msg =
         Format.sprintf "Add (%s, %s), %d %d ==> %d; Red, Green"
           id_t1 id_t2 (value_of r1) (value_of r2) (n1 + n2)
       in Logger.debug msg;
       Not_specialized (Add (id_t1, C (n2)), Red (n1 + n2))
     | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
       let msg =
         Format.sprintf "Add (%s, %s), %d %d ==> %d; Green, Red"
           id_t1 id_t2 (value_of r1) (value_of r2) (n1 + n2)
       in Logger.debug msg;
       let id_t' = match id_or_imm with
           V (id) -> id
         | C (n) -> failwith "Add (green, red)"
       in
       Not_specialized (Add (id_t', C (n1)), Red (n1 + n2))
     | Red (n1), Red (n2) ->
       let msg = Format.sprintf "Add (%s, %s), %d %d ==> %d; Red, Red"
           id_t1 id_t2 (value_of r1) (value_of r2) (n1 + n2)
       in Logger.debug msg;
       Not_specialized (exp, Red (n1 + n2)))
  | Sub (id_t1, id_or_imm) as exp ->
    let r1 = reg.(int_of_id_t id_t1) in
    let r2 = match id_or_imm with
      | V (id_t) -> reg.(int_of_id_t id_t)
      | C (n) -> Green (n)
    in
    (match r1, r2 with
     | Green (n1), Green (n2)
     | LightGreen (n1), LightGreen (n2)
     | LightGreen (n1), Green (n2)
     | Green (n1), LightGreen (n2) ->
       let msg = Format.sprintf "Sub (%s, %s), %d %d; Red, Red" id_t1 (string_of_id_or_imm id_or_imm) n1 n2 in
       Logger.debug msg;
       Specialized (Green (n1 - n2))
     | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
       Not_specialized (Sub (id_t1, C (n2)), Red (n1 - n2))
     | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
       let id_t' = match id_or_imm with
           V (id) -> id
         | C (n) -> failwith "Sub (green, red)"
       in
       Not_specialized (Add (id_t', C (n1)), Red (n1 - n2))
     | Red (n1), Red (n2) ->
       Not_specialized (exp, Red (n1 - n2)))
  | Ld (id_t, id_or_imm, x) as exp ->
    let destld = reg.(int_of_id_t id_t) in
    let offsetld =
      (match id_or_imm with
       | V (id_t) ->
         (match reg.(int_of_id_t id_t) with
          | Green (n1) | LightGreen (n1) -> Green (n1 * x)
          | Red (n1) -> Red (n1 * x))
       | C (n) -> Green (n * x))
    in
    let id_t2 =
      match id_or_imm with
        V (id) -> id
      | C (n) -> string_of_int n
    in
    (match destld, offsetld with
     | Green (n1), Green (n2)
     | LightGreen (n1), LightGreen (n2)
     | LightGreen (n1), Green (n2)
     | Green (n1), LightGreen (n2) ->
       begin match mem.(n1 + n2) with
         | Green n | LightGreen n as value ->
           let msg =
             Format.sprintf "Ld (%s, %s), %d %d => %d (Green): Green, Green"
               id_t id_t2 (value_of destld) (value_of offsetld) n
           in Logger.debug msg;
           Specialized (value)
         | Red n ->
           let msg =
             Format.sprintf "Ld (%s, %s), %d %d => %d (Red): Green, Green"
               id_t id_t2 (value_of destld) (value_of offsetld) n
           in Logger.debug msg;
           let e = Ld (zero, C (n1 + n2), 0) in
           Not_specialized (e, Red n)
       end
     | Green (n1), Red (n2) | LightGreen (n1), Red (n2) -> failwith "Ld (green, red)"
     | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
       let n = mem.(n1 + n2) in
       let msg =
         Format.sprintf "Ld (%s, %s), %d %d => %d: Red, Green"
           id_t id_t2 (value_of destld) (value_of offsetld) (value_of n)
       in Logger.debug msg;
       begin match mem.(n1 + n2) with
         | Green (n) | LightGreen (n) ->
           Not_specialized (Ld (id_t, C (n2), x), Red (n))
         | Red (n) ->
           Not_specialized (Ld (id_t, C (n2), x), Red (n))
       end
     | Red (n1), Red (n2) ->
       let n = mem.(n1 + n2) in
       let msg =
         Format.sprintf "Ld (%s, %s), %d %d => %d: Red, Red"
           id_t id_t2 (value_of destld) (value_of offsetld) (value_of n)
       in Logger.debug msg;
       Not_specialized (exp, Red (value_of n)))
  | St (src, dest, offset, x) ->
    let src' = reg.(int_of_id_t src) in
    let dest' = reg.(int_of_id_t dest) in
    let offset' = match offset with
      | V (id_t) ->
        (match reg.(int_of_id_t id_t) with
         | Green (n) | LightGreen (n) -> Green (n * x)
         | Red (n) -> Red (n * x))
      | C (n) -> Green (n * x)
    in
    begin match dest', offset' with
      | Green (n1), Green (n2)
      | LightGreen (n1), LightGreen (n2)
      | LightGreen (n1), Green (n2)
      | Green (n1), LightGreen (n2) ->
        begin match src' with
          | Green (n) | LightGreen (n) ->
            mem.(n1 + n2) <- src';
            let msg =
              Format.sprintf
                "St (%s, %s, %s, %d), %d %d %d: Green, Green, Green"
                src dest (string_of_id_or_imm offset) x (value_of src') (value_of dest') (value_of offset')
            in Logger.debug msg;
            Specialized (Green (0))
          | Red (n) ->
            let msg =
              Format.sprintf
                "St (%s, %s, %s, %d), %d %d %d: Green, Green, Red"
                src dest (string_of_id_or_imm offset) x (value_of src') (value_of dest') (value_of offset')
            in Logger.debug msg;
            Not_specialized (St (src, zero, C (n1 + n2), 0), Red (n))
        end
      | Green (n1), Red (n2) | LightGreen (n1), Red (n2) ->
        failwith "St (green, red)"
      | Red (n1), Green (n2) | Red (n1), LightGreen (n2) ->
        begin match src' with
          | Green (n) | LightGreen (n) ->
            mem.(n1 + n2) <- src';
            let msg =
              Format.sprintf
                "St (%s, %s, %s, %d), %d %d %d: Red, Green, Green"
                src dest (string_of_id_or_imm offset) x (value_of src') (value_of dest') (value_of offset')
            in Logger.debug msg;
            Not_specialized (St (src, dest, C (n2), x), Red (0))
          | Red (n) ->
            mem.(n1 + n2) <- src';
            let msg =
              Format.sprintf
                "St (%s, %s, %s, %d), %d %d %d: Red, Green, Red"
                src dest (string_of_id_or_imm offset) x (value_of src') (value_of dest') (value_of offset')
            in Logger.debug msg;
            Not_specialized (St (src, zero, C (n1 + n2), x), Red (0))
        end
      | Red (n1), Red (n2) ->
        begin match src' with
          | Green (n) | LightGreen (n) ->
            mem.(n1 + n2) <- Red (value_of src');
            let msg =
              Format.sprintf
                "St (%s, %s, %s, %d), %d %d %d: Red, Red, Green"
                src dest (string_of_id_or_imm offset) x (value_of src') (value_of dest') (value_of offset')
            in Logger.debug msg;
            Not_specialized (St (src, dest, C (n2), x), Red (0))
          | Red (n) ->
            mem.(n1 + n2) <- src';
            let msg =
              Format.sprintf
                "St (%s, %s, %s, %d), %d %d %d: Red, Red, Red"
                src dest (string_of_id_or_imm offset) x (value_of src') (value_of dest') (value_of offset')
            in Logger.debug msg;
            Not_specialized (St (src, zero, C (n1 + n2), x), Red (0))
        end
    end
  | _ ->
    show_exp e
    |> Printf.sprintf "%s is not supported in optimization."
    |> fun s -> raise @@ Not_optimization_supported s
