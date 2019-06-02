type pc = int
type count_tbl =  (int, int) Hashtbl.t
type compiled_tbl =  (int, bool) Hashtbl.t

val get_count_hash : unit -> count_tbl
val get_compiled_hash : unit -> compiled_tbl

val count_up : int -> unit
val not_compiled : int -> bool
val has_compiled : int -> unit
val over_threshold : int -> bool

type content = Content of int * string
type trace_tbl = (int, string) Hashtbl.t

val get_trace_hash : unit -> trace_tbl
val register : content -> unit
val find_opt : int -> string option
