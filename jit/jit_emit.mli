open Mincaml
val emit_trace : Asm.fundef -> string -> string -> unit
val emit_trace' : fundef:Asm.fundef -> fname:string -> inameo:string -> inamen:string -> unit
