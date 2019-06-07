open MinCaml

type 'a env = {
  out : string;
  jit_typ : 'a;
  prog : Asm.prog;
}

val emit_dynamic : [< `Meta_tracing | `Meta_method] env -> Asm.fundef list -> unit

val emit : ?midflg:bool -> [< `Meta_tracing | `Meta_method] env -> Asm.fundef list -> unit
