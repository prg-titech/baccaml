module Array = struct
  include Array

  let print_array f arr =
    print_string "[|";
    Array.iter (fun a -> f a; print_string "; ") arr;
    print_string "|]"
end
