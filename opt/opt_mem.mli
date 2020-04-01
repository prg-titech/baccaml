open MinCaml
open Asm
open Opt_lib

val remove_rw : int M.t -> string M'.t -> t -> t
val find_remove_candidate : int M.t -> (string * exp) M'.t -> exp M.t -> t -> exp M.t
val remove_unread_write : exp M.t -> t -> t
val const_fold_rw : t -> t
