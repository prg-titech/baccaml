let counter = ref 0
let genid id =
  incr counter;
  Printf.sprintf "%s__%d" id !counter

let gentmp _ =
  incr counter;
  Printf.sprintf "tmp__%d" !counter
