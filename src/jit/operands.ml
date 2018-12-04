open MinCaml.Asm

exception Error

let (|*|) e (n1, n2) = match e with
  | IfEq _ -> n1 = n2
  | IfLE _ -> n1 <= n2
  | IfGE _ -> n1 >= n2
  | _ -> raise Error

let (|%|) e (x, y, t1, t2) = match e with
  | IfEq _ -> IfEq (x, y, t1, t2)
  | IfLE _ -> IfLE (x, y, t1, t2)
  | IfGE _ -> IfGE (x, y, t1, t2)
  | IfFEq _ ->
    begin match y with
        V y -> IfFEq (x, y, t1, t2)
      | C _ -> raise Error
    end
  | IfFLE _ ->
    begin match y with
        V y -> IfFLE (x, y, t1, t2)
      | C _ -> raise Error
    end
  | _ -> raise Error

let (|%%|) e (t1, t2) = match e with
  | IfEq (id_t, id_or_imm, _, _) -> IfEq (id_t, id_or_imm, t1, t2)
  | IfLE (id_t, id_or_imm, _, _) -> IfLE (id_t, id_or_imm, t1, t2)
  | IfGE (id_t, id_or_imm, _, _) -> IfGE (id_t, id_or_imm, t1, t2)
  | _ -> raise Error
