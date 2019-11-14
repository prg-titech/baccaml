open MinCaml

val annotate_fundef : [< `Meta_method | `Meta_tracing ] -> Asm.fundef -> Asm.fundef
val annotate : [< `Meta_method | `Meta_tracing ] -> Asm.prog -> Asm.prog
