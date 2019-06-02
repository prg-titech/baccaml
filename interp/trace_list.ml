type pc = int

type count_tbl =  (pc, int) Hashtbl.t

type compiled_tbl =  (int, bool) Hashtbl.t

let threshold = 2

let count_hash : count_tbl = Hashtbl.create 100

let compiled_hash : compiled_tbl = Hashtbl.create 100

let get_count_hash () = Hashtbl.copy count_hash

let get_compiled_hash () = Hashtbl.copy compiled_hash

let count_up pc =
  match Hashtbl.find_opt count_hash pc with
  | Some v ->
     Hashtbl.replace count_hash pc (v + 1)
  | None ->
     Hashtbl.add count_hash pc 1

let not_compiled (pc : int) : bool =
  match Hashtbl.find_opt compiled_hash pc with
  | Some _ -> false
  | None -> true

let has_compiled (pc : int) : unit =
  match Hashtbl.find_opt compiled_hash pc with
  | Some v -> Hashtbl.replace compiled_hash pc true
  | None -> Hashtbl.add compiled_hash pc true

let over_threshold (pc : int) : bool =
  match Hashtbl.find_opt count_hash pc with
  | Some count ->
     if count > threshold then
       true
     else
       false
  | None ->
     false

type content = Content of int * string

type trace_tbl = (int, string) Hashtbl.t

let trace_hash : trace_tbl = Hashtbl.create 100

let get_trace_hash () = Hashtbl.copy trace_hash

let register (Content (pc, name)) =
  match Hashtbl.find_opt trace_hash pc with
  | Some (v) -> ()
  | None -> Hashtbl.add trace_hash pc name

let find_opt (pc : int) : string option =
  Hashtbl.find_opt trace_hash pc
