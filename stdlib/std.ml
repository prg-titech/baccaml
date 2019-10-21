let (%>) f g x = g (f x)
let ($) f g x = f (g x)

module Array = struct
  include Array

  let string_of_array f arr =
    "[|" ^ (arr |> Array.to_list |> List.map f |> String.concat "; ") ^ "|]"

  let print_array f arr =
    print_string "[|";
    Array.iter (fun a -> f a; print_string "; ") arr;
    print_string "|]"
end

module ExString : sig
  (* get first element xxx of a string such as xxx.yy.zz *)
  val get_name : string -> string
  (* get last element zz of a string such as xxx.yy.zz *)
  val get_extension : string -> string
  (* check whether s1 has substring s2 *)
  val contains : string -> string -> bool
  (* check wheter s1 starts with substring s2 *)
  val starts_with : string -> string -> bool
end = struct
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

  let starts_with s1 s2 =
    let re = Str.regexp_string s2 in
    try
      if Str.search_forward re s1 0 = 0
      then true
      else false
    with Not_found -> false

  let _ =
    assert (starts_with "min_caml_print_int" "min_caml");
    assert (not (starts_with "__min_caml_print_int" "min_caml"))
end

module String = struct
  include String
  include ExString
end

module ExList : sig
  (* remove duplicated elements in a given list *)
  val unique : 'a list -> 'a list
  (* get a last element of list l *)
  val last : 'a list -> 'a
  (* get the index of an element elem in list lst *)
  val index : 'a -> 'a list -> int
  (* get the index of an element elem in list lst *)
  val index_opt : 'a -> 'a list -> int option
end = struct
  let rec unique list =
    let rec go l s =
      match l with
      | [] -> s
      | first :: rest ->
         if List.exists (fun e -> e = first) s
         then go rest s
         else go rest (s @ [first])
    in go list []

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


module List = struct
  include List
  include ExList
end
