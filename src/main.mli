val limit : int ref
val iter : int -> KNormal.t -> KNormal.t
val virtualize : Lexing.lexbuf -> Asm.prog
val compile : out_channel -> Lexing.lexbuf -> unit
val string : string -> unit
val compile_exec : string -> unit
