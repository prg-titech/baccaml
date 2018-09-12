open MinCaml
val emit_fundef : Asm.fundef -> Buffer.t
val emit_trace : [< `Meta_method | `Meta_tracing ] -> Asm.fundef -> string -> string -> unit
val emit_result : prog:Asm.prog -> traces:Asm.fundef list -> file:string -> jit_type:[< `Meta_method | `Meta_tracing ] -> unit
