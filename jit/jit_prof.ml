open MinCaml

module Trace_name : sig
  type t = Trace_name of string

  val value : t -> string
  val gen : [< `Meta_tracing | `Meta_method ] -> t
  val gen_str : [< `Meta_tracing | `Meta_method ] -> string
end = struct
  type t = Trace_name of string

  let value = function Trace_name s -> s
  let counter = ref 0

  let gen typ =
    let mark = match typ with `Meta_tracing -> "tj" | `Meta_method -> "mj" in
    let name = "trace" ^ mark ^ string_of_int !counter in
    incr counter;
    Trace_name (Id.genid name)
  ;;

  let gen_str typ = gen typ |> value
end

module type Prof = sig
  val threshold : int
  val typ : [ `Meta_method | `Meta_tracing ]
end

module Make_prof (M_prof : Prof) = struct
  open M_prof

  exception Not_found_name_at of int

  type pc = int
  type name = string

  let count_tbl = Hashtbl.create 100
  let compiled_tbl = Hashtbl.create 100

  let register : pc * name -> unit =
   fun (pc, name) ->
    match Hashtbl.find_opt compiled_tbl pc with
    | Some name -> ()
    | None -> Hashtbl.add compiled_tbl pc name
 ;;

  let count_up : pc -> unit =
   fun pc ->
    match Hashtbl.find_opt count_tbl pc with
    | Some count -> Hashtbl.replace count_tbl pc (count + 1)
    | None -> Hashtbl.add count_tbl pc 1
 ;;

  let find : pc -> name =
   fun pc ->
    try Hashtbl.find compiled_tbl pc with
    | Not_found -> raise (Not_found_name_at pc)
 ;;

  let find_opt : pc -> name option = fun pc -> Hashtbl.find_opt compiled_tbl pc

  let over_threshold : pc -> bool =
   fun pc ->
    match Hashtbl.find_opt count_tbl pc with
    | Some count -> count >= threshold
    | None -> false
 ;;

  let update : pc -> name -> unit =
   fun pc name -> Hashtbl.replace compiled_tbl pc name
 ;;

  let gen_name _ = Trace_name.(gen typ |> value)

  let%test _ =
    register (1, "tracetj0");
    for i = 1 to threshold + 10 do
      count_up 1
    done;
    over_threshold 1 = true
  ;;
end

module Method_prof = Make_prof (struct
  let threshold = 0
  let typ = `Meta_method
end)

module Trace_prof = struct
  include Make_prof (struct
    (* fib: let threshold = 2 *)
    (* tak: let threshold = 0 *)
    let threshold =
      Option.(
        bind (Sys.getenv_opt "THOLD_TJ") (fun pc_str ->
            int_of_string_opt pc_str)
        |> value ~default:0)
    ;;

    let typ = `Meta_tracing
  end)

  module Guard = struct
    let threshold =
      Option.(
        bind (Sys.getenv_opt "THOLD_GUARD") (fun v_str ->
            int_of_string_opt v_str)
        |> value ~default:100)
    ;;

    type guard_count = Guard_count of int

    let count_tbl : (int, guard_count) Hashtbl.t = Hashtbl.create 100
    let name_tbl : (int, string) Hashtbl.t = Hashtbl.create 100

    let count_up pc =
      match Hashtbl.find_opt count_tbl pc with
      | Some (Guard_count count) ->
        if pc <> 91  then
        Hashtbl.replace count_tbl pc (Guard_count (count + 1))
      | None -> Hashtbl.add count_tbl pc (Guard_count 1)
    ;;

    let over_threshold pc =
      match Hashtbl.find_opt count_tbl pc with
      | Some (Guard_count count) -> count > threshold
      | None -> false
    ;;

    let register_name (pc, name) =
      if not (Hashtbl.mem name_tbl pc) then Hashtbl.add name_tbl pc name
    ;;

    let find_name_opt pc = Hashtbl.find_opt name_tbl pc
    let mem_name pc = Hashtbl.mem name_tbl pc
  end
end
