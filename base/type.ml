type t =
  (* MinCamlの型を表現するデータ型 (caml2html: type_t) *)
  | Unit
  | Bool
  | Int
  | Float
  | String
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref
[@@deriving show]

let rec type_of_string : string -> t =
 fun x ->
  match x with
  | "unit" | "Unit" -> Unit
  | "int" | "Int" -> Int
  | "float" | "Float" -> Float
  | "string" | "String" -> String
  | "arr" | "array" | "Array" -> Array Int
  | "arrint" | "arrayint" | "ArrayInt" -> Array Int
  | "arrfloat" | "arrayfloat" | "ArrayFloat" -> Array Float
  | _ -> failwith @@ Printf.sprintf "un matched pattern, %s" x
;;

let rec print_type = function
  | Unit -> print_string "Unit"
  | Bool -> print_string "Bool"
  | Int -> print_string "Int"
  | Float -> print_string "Float"
  | String -> print_string "String"
  | Fun (xs, x) ->
    print_string "Fun (";
    xs
    |> List.iter (fun t ->
           print_type t;
           print_string " -> ");
    print_type x;
    print_string ")"
  | Tuple xs ->
    print_string "Tuple (";
    xs
    |> List.iter (fun t ->
           print_type t;
           print_string ", ");
    print_string ")"
  | Array t ->
    print_string "Array (";
    print_type t;
    print_string ")"
  | Var t_opt ->
    (match !t_opt with
    | Some t ->
      print_string "Var (";
      print_type t;
      print_string ")"
    | None -> print_string "Var()")
;;

let gentyp () = Var (ref None) (* 新しい型変数を作る *)
