type t = string (* 変数の名前 (caml2html: id_t) *)
[@@deriving show]

type l = L of string (* トップレベル関数やグローバル配列のラベル (caml2html: id_l) *)
[@@deriving show]

let (=||=) id_l str = match id_l with L name -> name = str

let l_eq id_l str = id_l =||= str

let string_of_id_l = function L id -> "L " ^ id

let print_id_l = function L name -> print_string "L "; print_string name; print_string ""

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let counter = ref 0
let genid s =
  if s = "interp" then
    s
  else begin
    incr counter;
    Printf.sprintf "%s.%d" s !counter
  end

let const_counter = ref 0
let gen_const_id s =
  incr const_counter;
  Printf.sprintf "L__const.%d" !const_counter

let id_of_typ = function
  | Type.Unit -> "u"
  | Type.Bool -> "b"
  | Type.Int -> "i"
  | Type.Float -> "d"
  | Type.String -> "s"
  | Type.Fun _ -> "f"
  | Type.Tuple _ -> "t"
  | Type.Array _ -> "a"
  | Type.Var _ -> assert false
let gentmp typ =
  incr counter;
  Printf.sprintf "T%s%d" (id_of_typ typ) !counter
