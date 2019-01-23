open Core
open MinCaml
open Asm
open Jit_util

exception Error of string

type label = Id.l * int
type table = Id.l * float
type prog' = Prog' of table list * fundef list * t * label list

type res =
  | Jit of fundef list
  | Value of int

let value_of_res = function
  | Value (n) -> n
  | _ -> assert false

let regsize = 1000000
let heap = ref 0

let counter = ref 0

module Util = struct

  let prog'_of_prog prog =
    let rec create_labels fundefs i =
      match fundefs with
      | [] -> []
      | fundef :: tl -> (fundef.name, i) :: create_labels tl (i + 1)
    in
    let Prog (table, fundefs, exp) = prog in
    let labels = create_labels fundefs 0 in
    Prog' (table, fundefs, exp, labels)

  let rec find_label_number label = function
    | [] -> let Id.L s = label in int_of_id_t s
    | (l, num) :: tl -> if l = label then num else find_label_number label tl

  let rec find_label prog num =
    let Prog' (_, _, _, labels) = prog in
    match List.find ~f:(fun (id_l, n) -> n = num) labels with
    | Some (id, _) -> id
    | None ->
      Logs.err (fun m -> m "num: %d" num);
      failwith (Printf.sprintf "Not found num: %d" num)

  let rec lookup_by_id_l prog name =
    let Prog' (_, fundefs, _, _) = prog in
    match List.find ~f:(fun fundef -> (fundef.name = name)) fundefs with
    | Some (fundef) -> fundef
    | None ->
      Logs.err (fun m -> let Id.L s = name in m "CallCls %s is not found" s);
      assert false

  let rec lookup_by_id_t prog name =
    let Prog' (_, fundefs, _, _) = prog in
    match List.find ~f:(fun fundef -> (let Id.L s = fundef.name in s) = name) fundefs with
    | Some (fundef) -> fundef
    | None ->
      Logs.err (fun m -> m "CallCls %s" name);
      assert false

  (* 仮引数のレジスタに実引数がしまわれている reg を作る *)
  let new_reg reg args_tmp args_real =
    let regs_tmp = List.map ~f:int_of_id_t args_tmp in
    let regs_real = List.map ~f:int_of_id_t args_real in
    let arr = Array.create regsize 0 in
    List.zip_exn regs_tmp regs_real
    |> List.iter ~f:(fun (x, y) -> arr.(x) <- reg.(y));
    arr

end

module Jit = struct

  let paint_colors_reg greens fvs reg =
    let reg' = Array.create ~len:(Array.length reg) (Red (0)) in
    List.iter fvs ~f:begin fun fv ->
      let fvn, fvi =
        let fv' = String.split ~on:'.' fv in
        List.hd_exn fv', List.last_exn fv' |> int_of_string
      in
      let n = reg.(fvi) in
      if List.exists greens ~f:(fun green -> green = fvn) then
        reg'.(fvi) <- Green (n)
      else
        reg'.(fvi) <- Red (n)
    end;
    reg'

  let paint_colors_mem mem =
    let mem' = Array.create ~len:(Array.length mem) (Green (0)) in
    Array.iteri mem ~f:begin fun i m ->
      mem'.(i) <- Green (mem.(i))
    end;
    mem'

  let create_state (Prog' (tables, fundefs, main, _)) reg mem =
    let p = Prog (tables, fundefs, main) in
    let { name; args; fargs; body = t; ret } = find_fundef' p "interp" in
    let fvs = Asm.fv t in
    let greens = ["bytecode"; "pc"] in
    let reg' = paint_colors_reg greens fvs reg in
    let mem' = paint_colors_mem mem in
    reg', mem'

end

let rec eval_t prog reg mem instr : res =
  match instr with
  | Ans exp ->
    let res = exp |> eval_exp prog reg mem  in
    Logs.debug (fun m -> m  "Ans (%d)" (res |> value_of_res));
    res
  | Let (("min_caml_hp", _), exp, t) ->
    let res = exp |> eval_exp prog reg mem |> value_of_res in
    Logs.debug (fun m -> m "Let (id: %s, reg_num: %d, res: %d)" "min_caml_hp" !heap res);
    heap := res;
    t |> eval_t prog reg mem
  | Let (_, CallDir (Id.L ("min_caml_can_enter_jit"), args, fargs), t) ->
     incr counter;
     if !counter > 100 then
       assert false
     else
       t |> eval_t prog reg mem
  | Let ((id, _), exp, t) ->
    let n = int_of_id_t id in
    let res = exp |> eval_exp prog reg mem |> value_of_res in
    Logs.debug (fun m -> m "Let (id: %s, reg_num: %d, res: %d)" id n res);
    reg.(n) <- res;
    t |> eval_t prog reg mem

and eval_exp prog reg mem e : res =
  match e with
  | Nop ->
    Logs.debug (fun m -> m "Nop");
    Value (0)
  | Set n ->
    Logs.debug (fun m -> m  "Set (%d)" n);
    Value (n)
  | Neg n ->
    let res = reg.(int_of_id_t n) in
    Logs.debug (fun m -> m  "Neg %d" res);
    Value (- res)
  | SetL id_l ->
    let Prog' (_, _, _, labels) = prog in
    let res = Util.find_label_number id_l labels in
    Logs.debug (fun m -> m  "SetL (%s: %d)" (let Id.L s = id_l in s) res);
    Value (res)
  | Mov "min_caml_hp" ->
    Logs.debug (fun m -> m  "Mov (min_caml_hp: %d)" !heap);
    Value (!heap)
  | Mov id_t ->
    let n = int_of_id_t id_t in
    let res = reg.(n) in
    Logs.debug (fun m -> m  "Mov (id_t: %s, regnum: %d, res: %d)" id_t n res);
    Value (res)
  | Add (id_t1, V (id_t2)) ->
    (match id_t1 with
     | "min_caml_hp" ->
       let r1 = !heap in
       let r2 = int_of_id_t id_t2 in
       let res = r1 + reg.(r2) in
       Logs.debug (fun m -> m  "Add (r1: %d, r2: %d, res: %d)" r1 r2 res);
       Value (res)
     | _ ->
       let r1 = int_of_id_t id_t1 in
       let r2 = int_of_id_t id_t2 in
       let res = reg.(r1) + reg.(r2) in
       Logs.debug (fun m -> m  "Add (r1: %d, r2: %d, res: %d)" r1 r2 res);
       Value (res))
  | Add (id_t, C n) ->
    (match id_t with
     | "min_caml_hp" ->
       let r1 = !heap in
       let res = r1 + n in
       Logs.debug (fun m -> m  "AddImm (r1: %d, r2: %d, res: %d)" r1 n res);
       Value (res)
     | _ ->
       let r1 = int_of_id_t id_t in
       let res = reg.(r1) + n in
       Logs.debug (fun m -> m  "AddImm (r1: %d, r2: %d, res: %d)" r1 n res);
       Value (res))
  | Sub (id_t1, V (id_t2)) ->
    (match id_t1 with
       "min_caml_hp" ->
       let r1 = !heap in
       let r2 = reg.(int_of_id_t id_t2) in
       let res = r1 - r2 in
       Logs.debug (fun m -> m  "Sub (r1: %d, r2: %d, res: %d)" r1 r2 res);
       Value (res)
     | _ ->
       let r1 = int_of_id_t id_t1 in
       let r2 = int_of_id_t id_t2 in
       let res = reg.(r1) - reg.(r2) in
       Logs.debug (fun m -> m  "Sub (r1: %d, r2: %d, res: %d)" r1 r2 res);
       Value (res))
  | Sub (id_t, C (n)) ->
    (match id_t with
       "min_caml_hp" ->
       let r1 = !heap in
       let res = r1 - n in
       Logs.debug (fun m -> m  "SubImm (r1: %d, r2: %d, res: %d)" r1 n res);
       Value (res)
     | _ ->
       let r1 = int_of_id_t id_t in
       let res = reg.(r1) - n in
       Logs.debug (fun m -> m  "SubImm (r1: %d, r2: %d, res: %d)" r1 n res);
       Value (res))
  | Ld (id_t, id_or_imm, x) ->
    (* id_t + id_or_imm * x の番地から load *)
    let dest = match id_t with
      | "min_caml_hp" -> !heap
      | _ -> reg.(int_of_id_t id_t)
    in
    let offset = (match id_or_imm with
        | V id_t -> reg.(int_of_id_t id_t)
        | C n -> n) * x
    in
    let res = mem.(dest + offset) in
    Logs.debug (fun m -> m  "Ld (id_t: %s, dest: %d, offset: %d, m: %d, res: %d)" id_t dest offset (dest + offset) res);
    Value (res)
  | St (id_t1, id_t2, id_or_imm, x) ->
    (* id_t2 + id_or_imm * x の番地に id_t1 を store *)
    let src = match id_t1 with
        "min_caml_hp" -> !heap
      | _ -> reg.(int_of_id_t id_t1)
    in
    let dest = match id_t2 with
        "min_caml_hp" -> !heap
      | _ -> reg.(int_of_id_t id_t2)
    in
    let offset = (match id_or_imm with
        | V "min_caml_hp" -> !heap
        | V id_t -> reg.(int_of_id_t id_t)
        | C n -> n) * x
    in
    let m' = dest + offset in
    Logs.debug (fun m -> m  "St (id_t1: %s, id_t2: %s, dest: %d, offset: %d, m: %d), res: %d" id_t1 id_t2 dest offset m' src);
    mem.(m') <- src;
    Value (0)
  | IfEq (id1, id_or_imm, t1, t2) ->
    let r1 = match id1 with "min_caml_hp" -> !heap | _ -> reg.(int_of_id_t id1) in
    let r2 = match id_or_imm with
        V (id_t) -> reg.(int_of_id_t id_t)
      | C (i) -> i
    in
    Logs.debug (fun m -> m  "IfEq (id1: %s, id2: %s, t1: %d, t2: %d)" id1 (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 = r2 then
      t1 |> eval_t prog reg mem
    else
      t2 |> eval_t prog reg mem
  | IfLE (id, id_or_imm, t1, t2) ->
    let r1 = match id with "min_caml_hp" -> !heap | _ -> reg.(int_of_id_t id) in
    let r2 = match id_or_imm with
        V (id_t) -> reg.(int_of_id_t id_t)
      | C (i) -> i
    in
    Logs.debug (fun m -> m  "IfLE (id: %s, id_or_imm: %s, t1: %d, t2: %d)" id (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 <= r2 then
      t1 |> eval_t prog reg mem
    else
      t2 |> eval_t prog reg mem
  | IfGE (id, id_or_imm, t1, t2) ->
    let r1 = match id with "min_caml_hp" -> !heap | _ -> reg.(int_of_id_t id) in
    let r2 = match id_or_imm with
        V (id_t) -> reg.(int_of_id_t id_t)
      | C (i) -> i
    in
    Logs.debug (fun m -> m  "IfGE (id1: %s, id2: %s, t1: %d, t2: %d)" id (string_of_id_or_imm id_or_imm) r1 r2);
    if r1 >= r2 then
      t1 |> eval_t prog reg mem
    else
      t2 |> eval_t prog reg mem
  | CallCls (id_t, args, _) ->
    let r1 = reg.(int_of_id_t id_t) in
    let m1 = mem.(r1) in
    let Id.L id = Util.find_label prog m1 in
    Logs.debug (fun m -> m  "CallCls (id_t: %s, r1: %d, r2: %d, id_l: %s)" id_t r1 m1 id);
    let { args = args'; body } = Util.lookup_by_id_l prog (Id.L (id)) in
    let reg' = Util.new_reg reg args' args in
    reg'.(int_of_id_t id) <- r1;
    body |> eval_t prog reg' mem
  | CallDir (Id.L ("min_caml_print_int"), [arg], _) ->
    let v = reg.(int_of_id_t arg) in
    Logs.debug (fun m -> m  "CallDir min_caml_print_int %d" v);
    Out_channel.output_string stdout (string_of_int v);
    Value (0)
  | CallDir (Id.L ("min_caml_print_newline"), _, _) ->
    Out_channel.newline stdout;
    Value (0)
  | CallDir (Id.L ("min_caml_create_array"), arg1 :: arg2 :: [], _) ->
    let size = reg.(int_of_id_t arg1) in
    let init = reg.(int_of_id_t arg2) in
    let a = !heap in
    heap := !heap + (size * 4);
    for i = 0 to size do
      mem.(a + i * 4) <- init
    done;
    Logs.debug (fun m -> m  "CallDir (min_caml_create_array, arg1: %d, arg2: %d)" size init);
    Value (a)
  | CallDir (Id.L ("min_caml_read_int"), args, fargs) ->
    Logs.debug (fun m -> m "CallDir (read_int)");
    Out_channel.(flush stdout);
    Value (int_of_string In_channel.(input_line_exn stdin))
  | CallDir (name, args, _) ->
    (* { args' }: 仮引数 args: 実引数 *)
    let pc = Array.get (List.to_array args) 1 |> int_of_id_t |> Array.get reg in
    Logs.debug (fun m -> m "CallDir (%s), pc: %d" (let Id.L (x) = name in x) pc);
    let { args = args'; body } = Util.lookup_by_id_l prog name in
    let reg' = Util.new_reg reg args' args in
    let Id.L s = name in Logs.debug (fun m -> m  "CallDir %s" s);
    body |> eval_t prog reg' mem
  | _ ->
    raise (Error "Not implemented.")

let f prog =
  let reg = Array.create regsize 0 in
  let mem = Array.create regsize 0 in
  let prog' = Util.prog'_of_prog prog in
  let Prog' (_, _, t, labels) = prog' in
  match t |> eval_t prog' reg mem with
  | Jit _ -> assert false
  | Value n -> n
