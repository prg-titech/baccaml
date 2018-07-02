open MinCaml
open Asm
open Core
open Jit_config
open Jit_util
open Renaming

let rec inline_args argsr argst funbody reg =
  match argsr, argst with
  | [], [] ->
    funbody
  | hdr :: tlr, hdt :: tlt when hdr = hdt ->
    inline_args tlr tlt funbody reg
  | hdr :: tlr, hdt :: tlt ->
    reg.(int_of_id_t hdt) <- reg.(int_of_id_t hdr);
    Let ((hdt, Type.Int), Mov (hdr), (inline_args tlr tlt funbody reg))
  | _ ->
    failwith "Un matched pattern."

let rec inline_calldir_exp argsr fundef reg =
  let { args; body } = Renaming.rename_fundef fundef in
  inline_args argsr args body reg
