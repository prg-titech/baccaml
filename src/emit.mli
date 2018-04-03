val stackset : S.t ref
val stackmap : S.elt list ref
val save : string -> unit
val savef : S.elt -> unit
val locate : S.elt -> int list
val offset : string -> int
val stacksize : unit -> int
val pp_id_or_imm : Asm.id_or_imm -> string
type dest = Tail | NonTail of Id.t
val g : out_channel -> dest * Asm.t -> unit
val h : out_channel -> Asm.fundef -> unit
val f : out_channel -> Asm.prog -> unit
