open MinCaml
val emit_trace : [< `Meta_method | `Meta_tracing ] -> Asm.fundef -> string -> string -> unit
val emit_result : jit_type:[< `Meta_method | `Meta_tracing ] -> prog:Asm.prog -> traces:Asm.fundef list -> file:string ->  unit
