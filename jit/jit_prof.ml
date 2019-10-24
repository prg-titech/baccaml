open Base

module type Prof = sig
  val threshold : int
end

module Make_prof (M_prof : Prof) = struct
  open M_prof

  exception Not_found_name of string

  type pc = int
  type name = string
  let count_tbl = Hashtbl.create 1000
  let compiled_tbl = Hashtbl.create 1000

  let register : pc * name -> unit = fun (pc, name) ->
    match Hashtbl.find_opt compiled_tbl pc with
    | Some name -> ()
    | None -> Hashtbl.add compiled_tbl pc name

  let count_up : pc -> unit = fun pc ->
    match Hashtbl.find_opt count_tbl pc with
    | Some count ->
      Hashtbl.replace count_tbl pc (count + 1)
    | None -> Hashtbl.add count_tbl pc 1

  let find : pc -> name = fun pc ->
    try
      Hashtbl.find compiled_tbl pc
    with Not_found -> raise (Not_found_name (string_of_int pc))

  let find_opt : pc -> name option = fun pc ->
    Hashtbl.find_opt compiled_tbl pc

  let over_threshold : pc -> bool = fun pc ->
    match Hashtbl.find_opt count_tbl pc with
    | Some count -> count >= threshold
    | None -> false
end

module Method_prof = Make_prof(struct let threshold = 0 end)
module Trace_prof = Make_prof(struct let threshold = 1000 end)

module Trace_name : sig
  type t = Trace_name of string

  val gen : [< `Meta_tracing | `Meta_method] -> t
  val value : t -> string
end = struct
  type t = Trace_name of string

  let counter = ref 0

  let gen typ =
    let mark = match typ with
        `Meta_tracing -> "tj"
      | `Meta_method -> "mj"
    in
    let name = "trace" ^ mark ^ string_of_int !counter in
    incr counter;
    Trace_name (Id.genid name)

  let value = function Trace_name s -> s
end
