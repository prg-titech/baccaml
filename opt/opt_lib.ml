open MinCaml
open Asm
open Printf

module M = Map.Make (String)
module M' = Map.Make (Int)

let ep = eprintf
let sp = sprintf
let pp = printf

let ( <=> ) e (x, y, t1, t2) =
  match e with
  | IfEq _ -> IfEq (x, y, t1, t2)
  | IfLE _ -> IfLE (x, y, t1, t2)
  | IfGE _ -> IfGE (x, y, t1, t2)
  | _ -> failwith "unexpected expression."
;;

let contains2 var (id_t, id_or_imm) =
  let open Asm in
  var = id_t || match id_or_imm with C n -> false | V x -> var = x
;;

let contains3 var (id_t1, id_t2, id_or_imm) =
  let open Asm in
  var = id_t1 || var = id_t2 || match id_or_imm with C n -> false | V x -> var = x
;;

let contains var e =
  match e with
  | Nop -> false
  | Set _ -> false
  | SetL (Id.L x) -> x = var
  | Mov x -> x = var
  | Add (x, y) | Sub (x, y) | Mul (x, y) | Div (x, y) | Mod (x, y) -> contains2 var (x, y)
  | Ld (x, y, z) -> contains2 var (x, y)
  | St (x, y, z, w) -> contains3 var (x, y, z)
  | IfEq (x, y, _, _) | IfGE (x, y, _, _) | IfLE (x, y, _, _) -> contains2 var (x, y)
  | CallCls (x, args, fargs) -> var = x || List.mem var args || List.mem var fargs
  | CallDir (Id.L x, args, fargs) -> var = x || List.mem var args || List.mem var fargs
  | _ -> false
;;
