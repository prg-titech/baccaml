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

end
