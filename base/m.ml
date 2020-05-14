(* customized version of Map *)

module M = struct
  include Map.Make (struct
    type t = Id.t
    let compare = compare
  end)

  let rec find_greedy key env =
    let open Option in
    let rec find_greedy' key env =
      match find_opt key env with
      | Some v' -> find_greedy' v' env
      | None -> some key
    in
    match find_opt key env with
    | Some v' -> find_greedy' v' env
    | None -> none
  [@@ocamlformat "disable"]
end

include M

let add_list xys env = List.fold_left (fun env (x, y) -> add x y env) env xys
let add_list2 xs ys env = List.fold_left2 (fun env x y -> add x y env) env xs ys
let to_list m = M.fold (fun key elem acc -> acc @ [ key, elem ]) m []
