open MinCaml
open Asm
open Core
open Jit_util
open Renaming

let rec inline_args reg argsr argst funbody =
  match argsr, argst with
  | [], [] ->
    funbody
  | hdr :: tlr, hdt :: tlt when hdr = hdt ->
    inline_args reg tlr tlt funbody
  | hdr :: tlr, hdt :: tlt ->
    reg.(int_of_id_t hdt) <- reg.(int_of_id_t hdr);
    Let ((hdt, Type.Int), Mov (hdr), (inline_args reg tlr tlt funbody))
  | _ ->
    failwith "Un matched pattern."

let inline_fundef reg argsr fundef =
  let { args; body } = Renaming.rename_fundef fundef in
  inline_args reg argsr args body
