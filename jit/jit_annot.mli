open MinCaml

val annotate : [< `Meta_method | `Meta_tracing ] -> Asm.prog -> Asm.prog
