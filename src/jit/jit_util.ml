open MinCaml
open Asm
open Jit_config

let int_of_id_t id =
  if id = "min_caml_hp" then
    failwith ("int_of_id_t min_caml_hp is not supported.")
  else
    Core.(
      match
        String.split ~on:'.' id
        |> List.last
        |> fun opt -> Option.(opt >>| int_of_string)
      with
      | Some (i) -> i
      | _ ->
        match
          Stringext.split ~max:2 ~on:'u' id
          |> List.last
          |> fun opt -> Option.(opt >>| int_of_string)
        with
        | Some (i) -> i
        | _ -> failwith (Printf.sprintf "int_of_id_t (%s) is failed" id)
    )

let value_of = function
  | Red (n) | Green (n) | LightGreen (n) -> n

let is_red = function
  | Red _ -> true
  | _ -> false

let is_green = function
  | Green _ -> true
  | _ -> false

let is_light_green = function
  | LightGreen _ -> true
  | _ -> false

let string_of_id_or_imm = function
    V (id_t) -> id_t
  | C (n) -> string_of_int n

let string_of_id_l = function
  | Id.L (x) -> x

let string_of_value = function
  | Green (n) -> Printf.sprintf "Green (%d)" n
  | LightGreen (n) -> Printf.sprintf "LightGreen (%d)" n
  | Red (n) -> Printf.sprintf "Red (%d)" n

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with _ -> false

let find_pc_addr (argsr : Id.t list) (jargs : method_jit_args) =
  match List.nth_opt argsr (jargs.pc_place) with
  | Some (v) -> int_of_id_t v
  | None -> failwith "find_pc in Method_jit is failed."

let find_pc_addr_with_p (p : prog) (jargs : method_jit_args) =
  let Prog (_, fundefs, _) = p in
  let { args } =
    match List.find_opt (fun { name } ->
        contains (string_of_id_l name) "interp") fundefs
    with
    | Some (fundef) -> fundef
    | None -> failwith "find fundef is failed in method jit."
  in
  find_pc_addr args jargs

let find_pc_with_p (p : prog) (reg : reg) (jargs : method_jit_args) =
  let Prog (_, fundefs, _) = p in
  let { args } =
    match List.find_opt (fun { name } ->
        contains (string_of_id_l name) "interp") fundefs
    with
    | Some (fundef) -> fundef
    | None -> failwith "find fundef is failed in method jit."
  in
  find_pc_addr args jargs |> Array.get reg |> value_of

let rec find_fundef prog name =
  let Asm.Prog (_, fundefs, _) = prog in
  match List.find_opt (fun fundef -> fundef.name = name) fundefs with
  | Some (body) -> body
  | None -> failwith "find_fundef in Method jit is failed"

let jit_value_of_id_t reg id_t = reg.(int_of_id_t id_t)

let jit_value_of_id_or_imm reg = function
  | V (id) -> reg.(int_of_id_t id)
  | C (n) -> Green (n)

let name_of id =
  try List.hd (String.split_on_char '.' id) with
  | _ -> id

let is_opcode id =
  contains (name_of id) "instr"

let _ =
  assert (is_opcode "instr")

let rec connect id_t instr body =
  let rec go id_t instr body = match instr with
    | Let (a, Nop, t) -> go id_t t body
    | Let (a, e, t) -> Let (a, e, go id_t t body)
    | Ans (Nop) -> body
    | Ans e -> Let ((id_t, Type.Int), e, body)
  in go id_t instr body
