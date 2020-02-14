open MinCaml
open Closure
open Jit_util

type value =
    Red of int
  | Green of int

type jit_result =
    Spec of value
  | Not_spec of t * value

let f p reg mem e = match e with
  | Unit -> Spec (Green 0)
  | Int n -> Spec (Green n)
  | Var x ->
    (match reg.(int_of_id_t x) with
     | Green n -> Spec (Green n)
     | Red n -> Not_spec (e, Red n))
  | Neg x ->
    (match reg.(int_of_id_t x) with
     | Green n -> Spec (Green n)
     | Red n -> Not_spec (e, Red n))
  | Add (x, y) ->
    (match reg.(int_of_id_t x), reg.(int_of_id_t y) with
     | Green n1, Green n2 ->
       Spec (Green (n1 + n2))
     | Red n1, Green n2 ->
       Not_spec (e, Red (n1 + n2))
     | Green n1, Red n2 ->
       Not_spec (e, Red (n1 + n2))
     | Red n1, Red n2 ->
       Not_spec (e, Red (n1 + n2)))
  | Sub (x, y) ->
    (match reg.(int_of_id_t x), reg.(int_of_id_t y) with
     | Green n1, Green n2 ->
       Spec (Green (n1 - n2))
     | Red n1, Green n2 ->
       Not_spec (e, Red (n1 - n2))
     | Green n1, Red n2 ->
       Not_spec (e, Red (n1 - n2))
     | Red n1, Red n2 ->
       Not_spec (e, Red (n1 - n2)))
  | Mul (x, y) ->
    (match reg.(int_of_id_t x), reg.(int_of_id_t y) with
     | Green n1, Green n2 ->
       Spec (Green (n1 * n2))
     | Red n1, Green n2 ->
       Not_spec (e, Red (n1 * n2))
     | Green n1, Red n2 ->
       Not_spec (e, Red (n1 * n2))
     | Red n1, Red n2 ->
       Not_spec (e, Red (n1 * n2)))
  | Div (x, y) ->
    (match reg.(int_of_id_t x), reg.(int_of_id_t y) with
     | Green n1, Green n2 ->
       Spec (Green (n1 / n2))
     | Red n1, Green n2 ->
       Not_spec (e, Red (n1 / n2))
     | Green n1, Red n2 ->
       Not_spec (e, Red (n1 / n2))
     | Red n1, Red n2 ->
       Not_spec (e, Red (n1 / n2)))
  | Mod (x, y) ->
    (match reg.(int_of_id_t x), reg.(int_of_id_t y) with
     | Green n1, Green n2 ->
       Spec (Green (n1 mod n2))
     | Red n1, Green n2 ->
       Not_spec (e, Red (n1 mod n2))
     | Green n1, Red n2 ->
       Not_spec (e, Red (n1 mod n2))
     | Red n1, Red n2 ->
       Not_spec (e, Red (n1 mod n2)))
  | ExtArray (x) -> Not_spec (e, Red 0)
  | e -> failwith "Not supported."
