module Config : sig
  val file_name : string option ref
  val size : int
  val greens : string list
  val bc_tmp_addr : int
  val st_tmp_addr : int
  val pc_method_annot_inst: int

  val jit_flag : [`On | `Off] ref
end

val jit_exec : int -> int -> int -> unit

val jit_tracing_entry : int array -> int array -> int -> int -> int -> int -> unit

val jit_method_call : int array -> int array -> int -> int -> int -> int -> int
