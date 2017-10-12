val limit : int ref
val virtualize : Lexing.lexbuf -> Asm.prog
val compile : out_channel -> Lexing.lexbuf -> unit
val string : string -> unit
val compile_exec : string -> unit
val interp : Lexing.lexbuf -> unit
val interp_exec : string -> unit
