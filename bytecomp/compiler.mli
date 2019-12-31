open Syntax

val stack_hybridized : bool ref

val compile_funs : fundef list -> Insts.inst array

(* for tracing *)
val compile_fun : (var -> Syntax.fundef) -> fundef -> Insts.inst list

val compile_from_exp : Syntax.exp -> Insts.inst array

module Test : sig
  val print_insts : Insts.inst list -> unit
  val compile_from_exp : Syntax.exp -> Insts.inst array
end
