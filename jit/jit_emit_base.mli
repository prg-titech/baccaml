open Base

type 'a env = {
  out : string;
  jit_typ : 'a;
  prog : Asm.prog;
}

val emit : ?midflg:bool -> [< `Meta_tracing | `Meta_method] env -> Asm.fundef list -> unit
