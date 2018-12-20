type var = string

type inst = int

type exp =
  | Inst of inst
  | Var of string
  | Green of (var * inst)
  | Red of (var * inst)
  | Jit_type of var
  | Merge_pc of inst

let print_exp = function
  | Inst i -> Printf.printf "Inst (%d)" i
  | Var s -> Printf.printf "Var (%s)" s
  | Green (s, i) -> Printf.printf "Green (%s, %d)" s i
  | Red (s, i) -> Printf.printf "Red (%s, %d)" s i
  | Jit_type s -> Printf.printf "Jit_type (%s)" s
  | Merge_pc i -> Printf.printf "Merge_pc (%d)" i
