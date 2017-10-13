module ListUtil = struct
  let rec zip lst1 lst2 = match lst1, lst2 with
    | [], _ -> []
    | _, [] -> []
    | (x::xs), (y::ys) -> (x, y) :: (zip xs ys)
  let unzip lst =
    let f (l, s) (x, y) = (x::l, y::s) in
    List.fold_left f ([],[]) (List.rev lst)
end

module InterpUtil = struct
  open Asm
  let register_size = 10000
  let heap_pointer = register_size - 1
  (* TODO: Split して数字を取り出す実装ではなく *)
  (* レジスタ番号を string で与えるように実装を変更する *)
  let int_of_id_t = function
    | "min_caml_hp" -> heap_pointer
    | id ->
      let splitted = Str.split (Str.regexp_string ".") id in
      (match List.nth splitted 1 with
       | num -> int_of_string num
       | exception _ -> int_of_string (Str.string_after (List.hd splitted) 2))

  let float_of_id_t id =
    let splitted = Str.split (Str.regexp_string ".") id in
    match List.nth splitted 1 with
    | num -> float_of_string num
    | exception _ -> float_of_string (Str.string_after (List.hd splitted) 2)

  let int_of_id_or_imm = function V (id_t) -> int_of_id_t id_t | C (n) -> n

  let string_of_id_or_imm = function V (id_t) -> id_t | C (n) -> string_of_int n

  let rec lookup_by_id_l (prog : prog) (name : Id.l) : fundef =
    match prog with
    | Prog (_, fundefs, _) ->
      try
        List.find (fun (fundef) -> fundef.name = name) fundefs
      with Not_found ->
        let Id.L s = name in
        Logger.error s;
        raise Not_found

  let rec lookup_by_id_t (prog : prog) (name : Id.t) : fundef =
    let Prog (_, fundefs, _) = prog in
    try
      List.find (fun fundef -> (let Id.L s = fundef.name in s) = name) fundefs
    with e ->
      Logger.error (Printf.sprintf "CallCls %s" name); raise e

  (* 仮引数のレジスタに実引数がしまわれている reg_set を作る *)
  let make_reg_set (reg_set : 'a array) (args_tmp : Id.t list) (args_real : Id.t list) : 'a array =
    let regs_tmp = List.map int_of_id_t args_tmp in
    let regs_real = List.map int_of_id_t args_real in
    let arr = Array.make 10000 0 in
    ignore (
      List.map
        (fun (x, y) -> arr.(x) <- reg_set.(y))
        (ListUtil.zip regs_tmp regs_real));
    arr
end
