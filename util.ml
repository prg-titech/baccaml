module ListUtil = struct
  let rec zip lst1 lst2 = match lst1, lst2 with
    | [], _ -> []
    | _, [] -> []
    | (x::xs), (y::ys) -> (x, y) :: (zip xs ys)

  let unzip lst =
    let f (l, s) (x, y) = (x::l, y::s) in
    List.fold_left f ([],[]) (List.rev lst)

  let print_list f lst =
    let rec print_elements = function
      | [] -> ()
      | h::t -> f h; print_string ";"; print_elements t
    in
    print_string "[";
    print_elements lst;
    print_string "]";;
end

module ArrayUtil = struct
  let print_array f arr =
    let rec print_elements = function
      | [] -> ()
      | h :: t -> f h; print_string ";"; print_elements t
    in
    print_string "[|";
    print_elements (Array.to_list arr);
    print_string "|]";;
end

module List = struct
  include List
  include ListUtil
end

module Array = struct
  include Array
  include ArrayUtil
end
