open MinCaml

(* apply constant folding + def-use analysis *)
val exec : Asm.t -> Asm.t

(* apply constant folding + def-use analysis to fundef *)
val h : Asm.fundef -> Asm.fundef
