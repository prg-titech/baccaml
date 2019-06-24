module Array = struct
  include Array

  let string_of_array f arr =
    "[|" ^ (arr |> Array.to_list |> List.map f |> String.concat "; ") ^ "|]"

  let print_array f arr =
    print_string "[|";
    Array.iter (fun a -> f a; print_string "; ") arr;
    print_string "|]"
end

module String = struct
  include String

  let get_name x =
    x |> String.split_on_char '.' |> List.hd

  let get_extension x =
    x |> String.split_on_char '.' |> List.rev |> List.hd

  let contains s1 s2 =
    let re = Str.regexp_string s2 in
    try
      ignore (Str.search_forward re s1 0);
      true
    with Not_found -> false

end

module List = struct
  include List

  exception Bad_access

  let rec last = function
    | [] -> raise Bad_access
    | [x] -> x
    | hd :: tl -> last tl

  let index elem lst =
    let rec go elem lst i =
      match lst with
      | [] -> raise Not_found
      | hd :: tl ->
         if hd = elem then i
         else go elem tl (i + 1)
    in go elem lst 0

  let index_opt elem lst =
    let rec go elem lst i =
      match lst with
      | [] -> None
      | hd :: tl ->
         if hd = elem then Some (i)
         else go elem tl (i + 1)
    in go elem lst 0

end

module Try = struct
  type 'a t = Success of 'a | Failure of exn

  let create : ('a -> 'b) = fun f ->
    try Success (f)
    with e -> Failure (e)

  let map : ('a -> 'b) -> 'a t -> 'b t = fun f tr ->
    match tr with
    | Success (a) -> Success (f a)
    | Failure (exn) -> Failure (exn)

  let value : 'a t -> 'a = function
    | Success (a) -> a
    | Failure (exn) -> raise exn

  let is_success : 'a t -> bool = function
    | Success _ -> true
    | Failure _ -> false

  let is_failure : 'a t -> bool = function
    | Success _ -> false
    | Failure _ -> true
end
