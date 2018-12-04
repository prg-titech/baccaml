open MinCaml

val gen_mj : [< `Meta_method | `Meta_tracing ] -> Asm.prog -> Asm.prog
