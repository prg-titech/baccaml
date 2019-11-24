open Syntax

val stack_hybridized : bool ref

val compile_funs : fundef list -> VM.inst array

(* for tracing *)
val compile_fun : (var -> Syntax.fundef) -> fundef -> VM.inst list

val compile_from_exp : Syntax.exp -> VM.inst array

module Test : sig
  val print_insts : VM.inst list -> unit
  val compile_from_exp : Syntax.exp -> VM.inst array
end
