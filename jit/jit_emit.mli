open Base

val emit_trace :
  [< `Meta_method | `Meta_tracing ] ->
  Asm.fundef ->
  string ->
  string ->
  unit

val emit_result :
  ?midflg:bool ->
  jit_type:[< `Meta_method | `Meta_tracing ] ->
  out:string -> prog:Asm.prog -> traces:Asm.fundef list -> unit
