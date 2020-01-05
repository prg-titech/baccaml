val jit_exec : int -> int -> int -> int array -> unit

val jit_tracing_entry
  :  int array
  -> int array
  -> int
  -> int
  -> int
  -> int
  -> unit

val jit_method_call : int array -> int array -> int -> int -> int -> int -> int
val callbacks : unit -> unit
