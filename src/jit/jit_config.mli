type value = Red of int | Green of int
type reg = value array
type mem = value array
type jit_result = Specialized of value | Not_specialized of Asm.exp * value
type jit_args = {
  trace_name : string;
  reds : string list;
  greens : string list;
  loop_header : int;
  loop_pc_place : int;
}
type method_jit_args = {
  method_name : string;
  reds : string list;
  method_start : int;
  method_end : int;
  pc_place : int;
}
val dummy_jit_args : jit_args
val enable_jit : bool Core.ref
val value_of : value -> int
val is_red : value -> bool
val is_green : value -> bool
val int_of_id_t : Core.String.t -> int
val find_fundef : Asm.prog -> Id.l -> Asm.fundef
