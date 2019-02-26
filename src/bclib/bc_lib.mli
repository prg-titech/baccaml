open MinCaml
open Bc_jit

type env = {
  prog : Asm.prog;
  reg : Jit_util.value array;
  mem : Jit_util.value array;
  red_args : string list;
  ex_name : string;
  merge_pc : int;
  trace_name : string;
}

type arg = {
  file : string;
  ex_name : string;
  code : int array;
  annot : int array;
  reds : (string * int) list;
  greens : (string * int) list;
  merge_pc : int;
  trace_name : string;
}

type var = {
  redtbl : (string, int) Hashtbl.t;
  greentbl : (string, int) Hashtbl.t;
}

type tenv = {
  fundefs : Asm.fundef list;
  ibody : Asm.t;
}

val prepare_env : [< `Meta_method | `Meta_tracing] -> arg -> env

val annot : [< `Meta_method | `Meta_tracing ] -> Asm.prog -> Asm.prog
