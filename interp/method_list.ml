type t = (int, string) Hashtbl.t

let tbl : t = Hashtbl.create 100

let count_tbl : (int, int) Hashtbl.t = Hashtbl.create 100

let threshold = 100

let register (pc, name) =
  match Hashtbl.find_opt tbl pc with
  | Some name -> ()
  | None -> Hashtbl.add tbl pc name

let find_opt pc =
  Hashtbl.find_opt tbl pc

let count_up pc =
  match Hashtbl.find_opt count_tbl pc with
  | Some v -> Hashtbl.replace count_tbl pc (v + 1)
  | None -> Hashtbl.add count_tbl pc 1

let over_threshold pc =
  match Hashtbl.find_opt count_tbl pc with
  | Some v -> v > threshold
  | None -> false
