module type Prof = sig
  val threshold : int
end

module Make_prof (M_prof : Prof) = struct
  open M_prof

  type pc = int
  type name = string
  let count_tbl = Hashtbl.create 100
  let compiled_tbl = Hashtbl.create 100

  let register : pc * name -> unit = fun (pc, name) ->
    match Hashtbl.find_opt compiled_tbl pc with
    | Some name -> ()
    | None -> Hashtbl.add compiled_tbl pc name

  let count_up : pc -> unit = fun pc ->
    match Hashtbl.find_opt count_tbl pc with
    | Some count -> Hashtbl.replace count_tbl pc (count + 1)
    | None -> Hashtbl.add count_tbl pc 1

  let find_opt : pc -> name option = fun pc ->
    Hashtbl.find_opt compiled_tbl pc

  let over_threshold : pc -> bool = fun pc ->
    match Hashtbl.find_opt count_tbl pc with
    | Some count -> count >= threshold
    | None -> false
end
