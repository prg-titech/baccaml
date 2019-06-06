type pc = int
type name = string
type count_tbl =  (int, int) Hashtbl.t
type compiled_tbl =  (int, bool) Hashtbl.t

val get_count_hash : unit -> count_tbl
val get_compiled_hash : unit -> compiled_tbl

val count_up : pc -> unit
val not_compiled : pc -> bool
val make_compiled : pc -> unit
val over_threshold : pc -> bool

type content = Content of pc * name
type trace_tbl = (pc, name) Hashtbl.t

val get_trace_hash : unit -> trace_tbl
val register : content -> unit
val find_opt : int -> string option
