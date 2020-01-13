open Std
open MinCaml
open Asm
open Jit_util
open Jit_env

let value_of_id_t reg id_t = reg.(int_of_id_t id_t) |> value_of

let filter ~reds args =
  List.filter (fun arg -> List.mem (String.get_name arg) reds) args
;;

let rec inline_fun (reg : reg) argr fundef k =
  let { args; body } = fundef in
  let argt, body = Renaming.rename (args, body) in
  let rec loop reg argr argt =
    match argr, argt with
    | [], [] -> k reg body
    | hdr :: tlr, hdt :: tlt ->
      let v = reg.(int_of_id_t hdr) in
      reg.(int_of_id_t hdt) <- v;
      (match v with
      | Green n -> Let ((hdt, Type.Int), Set n, loop reg tlr tlt)
      | Red n -> Let ((hdt, Type.Int), Mov hdr, loop reg tlr tlt))
    | _ -> failwith "Un matched pattern."
  in
  loop reg argr argt
;;

let rec restore_greens reg vars k =
  match vars with
  | hd :: tl ->
    (match reg.(int_of_id_t hd) with
    | Green n -> Let ((hd, Type.Int), Set n, restore_greens reg tl k)
    | Red n -> restore_greens reg tl k)
  | [] -> k ()
;;

let rec setup_reg reg argt argr =
  match argt, argr with
  | [], [] -> ()
  | hdt :: tlt, hdr :: tlr ->
    reg.(int_of_id_t hdt) <- reg.(int_of_id_t hdr);
    setup_reg reg tlt tlr
  | _ -> assert false
;;

let ( <=> ) e (n1, n2) =
  match e with
  | SIfEq _ | IfEq _ -> n1 = n2
  | SIfLE _ | IfLE _ -> n1 <= n2
  | SIfGE _ | IfGE _ -> n1 >= n2
  | _ -> assert false
;;

let ( <|> ) e (id_t, id_or_imm, t1, t2) =
  match e with
  | SIfEq _ | IfEq _ -> IfEq (id_t, id_or_imm, t1, t2)
  | SIfLE _ | IfLE _ -> IfLE (id_t, id_or_imm, t1, t2)
  | SIfGE _ | IfGE _ -> IfGE (id_t, id_or_imm, t1, t2)
  | _ -> assert false
;;
