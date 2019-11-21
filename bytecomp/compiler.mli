open Syntax

val compile_funs : fundef list -> VM.inst array

(* for tracing *)
val compile_fun : (var -> int) -> fundef -> VM.inst list

val compile_from_exp : Syntax.exp -> VM.inst array

module Test : sig
  val print_insts : VM.inst list -> unit
  val compile_from_exp : Syntax.exp -> VM.inst array
end
