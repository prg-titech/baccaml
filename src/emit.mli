type dest = Tail | NonTail of Id.t
val g : out_channel -> dest * Asm.t -> unit
val h : out_channel -> Asm.fundef -> unit
val f : out_channel -> Asm.prog -> unit
