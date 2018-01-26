type value = Red of int | Green of int
type jit_result = Specialized of value | Not_specialized of Asm.exp * value
val not_specialised : Asm.exp -> jit_result
type jit_branch_result = Selected of Asm.t | Not_selected of Asm.exp
type jit_args = {
  trace_name : string;
  reds : string list;
  greens : string list;
  loop_header : int;
  loop_pc_place : int;
}
val dummy_jit_args : jit_args
val enable_jit : bool Core.ref
val value_of : value -> int
val is_red : value -> bool
val is_green : value -> bool
val int_of_id_t : Core.String.t -> int
val find_fundef : Asm.prog -> Id.l -> Asm.fundef
val find_pc : Core.String.t Core.List.t -> jit_args -> int
val find_a : Core.String.t Core.List.t -> int -> int
