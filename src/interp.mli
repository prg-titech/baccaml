type labels = (Id.l * int) list
type prog_with_label =
    ProgWithLabel of (Id.l * float) list * Asm.fundef list * Asm.t * labels
val to_prog_with_label : Asm.prog -> prog_with_label
val interp : prog_with_label -> Asm.t -> int array -> int array -> JitUtil.jit_args -> int
val f : Asm.prog -> int
