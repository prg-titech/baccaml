open Insts

val debug_flg : bool ref

val max_stack_depth : int

type value = Int' of int | Array' of value array
type stack = int * value array
val interp : int array -> int -> stack -> value

val int_of_inst : inst -> int
val string_of : inst -> string (* for debugging *)

type fundef_bin_t = int array
type fundef_asm_t = inst array
val run_bin : fundef_bin_t -> int
val run_asm : fundef_asm_t -> int

module Value : sig
  val int_of_value : value -> int
  val value_of_int : int -> value
  val value_of_array : value array -> value
end
