open MinCaml
open Asm
open Jit_util

let rec unique list =
  let rec go l s =  match l with
      [] -> s
    | first :: rest ->
      if List.exists (fun e -> e = first) s
      then go rest s
      else go rest (s @ [first])
  in go list []

let split str =
  let str' = (String.split_on_char '.' str) in
  List.hd str', (str' |> List.rev |> List.hd)

let rec get_free_vars = function
  | Ans (exp) -> get_free_vars' exp
  | Let ((dest, _), e, t) -> List.append (dest :: (get_free_vars' e)) (get_free_vars t)

and get_free_vars' = function
  | Mov (id) -> [id]
  | Add (id_t, V (id)) | Sub (id_t, V (id)) ->  id_t :: id :: []
  | Add (id_t, C _) | Sub (id_t, C _) -> id_t :: []
  | Ld (dest, V (offset), _) -> dest :: offset :: []
  | Ld (dest, C (_), _) -> dest :: []
  | St (src, dest, V (offset), _) -> src :: dest :: offset :: []
  | St (src, dest, C (_), _) -> src :: dest :: []
  | IfEq (id_t1, V (id_t2), _, _) | IfLE (id_t1, V (id_t2), _, _) | IfGE (id_t1, V (id_t2), _, _) -> id_t1 :: id_t2 :: []
  | IfEq (id_t1, C (_), _, _) | IfLE (id_t1, C (_), _, _) | IfGE (id_t1, C (_), _, _) -> id_t1 :: []
  | CallDir (id_l, args, fargs) -> List.append args fargs
  | _ -> []

let colorize_reg redstbl greenstbl reg fundef t =
  let { args } = fundef in
  let free_vars = get_free_vars t |> unique in (* x.11 :: y.12 :: ..  *)
  let free_vars_with_id = List.map (fun var -> split var) (free_vars @ args) in (* (x, 11) :: (y, 12) :: *)
  List.iter (fun (var, id) ->
      (try
         let value = Hashtbl.find redstbl var in
         reg.(int_of_string id) <- Red (value);
       with Not_found -> ());
      (try
         let value' = Hashtbl.find greenstbl var in
         reg.(int_of_string id) <- Green (value')
       with Not_found -> ());
    ) free_vars_with_id

let _ =
  let reds = Hashtbl.create 100 in
  Hashtbl.add reds "a" 0;
  Hashtbl.add reds "b" 10;
  let greens = Hashtbl.create 100 in
  Hashtbl.add greens "c" 2;
  Hashtbl.add greens "d" 3;
  let reg = Array.make 100 (Red (0)) in
  let t = Ans (Mov ("c.14")) in
  let fundef = { name = Id.L "test"; args = ["a.12"; "b.13"]; fargs = []; body = t; ret = Type.Unit } in
  colorize_reg reds greens reg fundef t;
  assert (reg.(12) = Red (0));
  assert (reg.(13) = Red (10));
  assert (reg.(14) = Green (2))

let colorize_pgm pgm bytecode mem =
  for i = 0 to (Array.length pgm - 1) do
    mem.(bytecode + 4 * i) <- Green (pgm.(i))
  done
