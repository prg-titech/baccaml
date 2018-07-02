open MinCaml
val emit_fundef : Asm.fundef -> Buffer.t
val emit_trace : Jit_config.trace_result-> string -> string -> unit
val emit_result_mj : prog:Asm.prog -> traces:Asm.fundef list -> file:string -> unit
