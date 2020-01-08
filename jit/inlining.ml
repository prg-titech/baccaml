open MinCaml
open Asm
open Jit_env
open Jit_util
open Renaming

let rec reg_set reg argsr argst =
  match argst, argsr with
  | [], [] -> ()
  | hdt :: tlt, hdr :: tlr ->
    reg.(int_of_id_t hdt) <- reg.(int_of_id_t hdr);
    reg_set reg tlt tlr
  | _ -> failwith "length of argsr and argst isn't equal"
;;

let rec inline_args reg argsr argst funbody =
  match argsr, argst with
  | [], [] -> funbody
  | hdr :: tlr, hdt :: tlt when hdr = hdt -> inline_args reg tlr tlt funbody
  | hdr :: tlr, hdt :: tlt ->
    reg.(int_of_id_t hdt) <- reg.(int_of_id_t hdr);
    (match reg.(int_of_id_t hdr) with
    | Red _ -> Let ((hdt, Type.Int), Mov hdr, inline_args reg tlr tlt funbody)
    | Green _ -> inline_args reg tlr tlt funbody)
  | _ -> failwith "Un matched pattern."
;;

let inline_fundef reg argsr fundef =
  let { args; body } = Renaming.rename_fundef fundef in
  inline_args reg argsr args body
;;

let inline_fundef' reg argsr fundef =
  let { args = argst; body } = Renaming.rename_fundef fundef in
  inline_args reg argsr argst body, reg
;;
