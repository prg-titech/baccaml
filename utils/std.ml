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

  let rec unique list =
    let rec go l s =
      match l with
      | [] -> s
      | first :: rest ->
         if List.exists (fun e -> e = first) s
         then go rest s
         else go rest (s @ [first])
    in go list []

  let rec filter_map cond f = function
    | [] -> []
    | hd :: tl when cond hd ->
       (f hd) :: (filter_map cond f tl)
    | hd :: tl -> filter_map cond f tl

  let rec last = function
    | [] -> failwith "last"
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

module Option = struct

  let get = function
    | Some v -> v
    | None -> failwith "Option#value None"

  let get_or_else ~cmp = function
    | Some v -> v
    | None -> cmp

  let or_else ~cmp = function
    | Some v -> Some v
    | None -> cmp

  let map f = function
    | Some v -> Some (f v)
    | None -> None

  let (>==) f opt =
    match opt with
    | Some (v) -> f v
    | None -> None
end
