type labels = (Id.l * int) list
type prog_interp =
    ProgInterp of (Id.l * float) list * Asm.fundef list * Asm.t * labels
val to_prog_interp : Asm.prog -> prog_interp
val f : Asm.prog -> unit
