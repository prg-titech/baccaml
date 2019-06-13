type t = (int, string) Hashtbl.t

val threshold : int

val register : int * string -> unit

val find_opt : int -> string option

val count_up : int -> unit

val over_threshold : int -> bool
