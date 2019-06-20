open Asm

exception Operation_failed of string

let (|*|) e (n1, n2) = match e with
  | IfEq _ | SIfEq _ -> n1 = n2
  | IfLE _ | SIfLE _ -> n1 <= n2
  | IfGE _ | SIfGE _ -> n1 >= n2
  | _ -> raise (Operation_failed "|*|")

let (|%|) e (id_t, id_or_imm, t1, t2) = match e with
  | IfEq _ -> IfEq (id_t, id_or_imm, t1, t2)
  | IfLE _ -> IfLE (id_t, id_or_imm, t1, t2)
  | IfGE _ -> IfGE (id_t, id_or_imm, t1, t2)
  | SIfEq _ -> SIfEq (id_t, id_or_imm, t1, t2)
  | SIfLE _ -> SIfLE (id_t, id_or_imm, t1, t2)
  | SIfGE _ -> SIfGE (id_t, id_or_imm, t1, t2)
  | _ -> raise (Operation_failed "|%|")

let (|%%|) e (t1, t2) = match e with
  | IfEq (id_t, id_or_imm, _, _) -> IfEq (id_t, id_or_imm, t1, t2)
  | IfLE (id_t, id_or_imm, _, _) -> IfLE (id_t, id_or_imm, t1, t2)
  | IfGE (id_t, id_or_imm, _, _) -> IfGE (id_t, id_or_imm, t1, t2)
  | SIfEq (id_t, id_or_imm, _, _) -> SIfEq (id_t, id_or_imm, t1, t2)
  | SIfLE (id_t, id_or_imm, _, _) -> SIfLE (id_t, id_or_imm, t1, t2)
  | SIfGE (id_t, id_or_imm, _, _) -> SIfGE (id_t, id_or_imm, t1, t2)
  | _ -> raise (Operation_failed "|%%|")
