open Asm
open Emit

module String = struct
  let empty = ""
end

let replace input output =
  Str.global_replace (Str.regexp_string input) output

(* 命令列のアセンブリ生成 as String *)
let rec create_asm = function
  | dest, Ans (exp) -> create_asm' (dest, exp)
  | dest, Let ((x, t), exp, e) ->
    create_asm' (NonTail(x), exp) ^ create_asm (dest, e)
(* 各命令のアセンブリ生成 as String *)
and create_asm' = function
  | NonTail (_), Nop -> ""
  | NonTail (x), Set (i) -> Printf.sprintf "\tmovl\t$%d, %s\n" i x
  | NonTail (x), SetL (Id.L (y)) -> Printf.sprintf "\tmovl\t$%s, %s\n" y x
  | NonTail (x), Mov (y) ->
    if x <> y then Printf.sprintf "\tmovl\t%s, %s\n" y x else String.empty
  | NonTail(x), Neg (y) ->
    (if x <> y then Printf.sprintf "\tmovl\t%s, %s\n" y x else String.empty) ^
    Printf.sprintf "\tnegl\t%s\n" x
  | NonTail (x), Add (y, z') ->
    if V(x) = z' then
      Printf.sprintf "\taddl\t%s, %s\n" y x
    else
      (if x <> y then Printf.sprintf "\tmovl\t%s, %s\n" y x else String.empty) ^
      Printf.sprintf "\taddl\t%s, %s\n" (pp_id_or_imm z') x
  | NonTail (x), Sub (y, z') ->
    if V (x) = z' then
      (Printf.sprintf "\tsubl\t%s, %s\n" y x ^
       Printf.sprintf "\tnegl\t%s\n" x)
    else
      (if x <> y then Printf.sprintf "\tmovl\t%s, %s\n" y x else String.empty) ^
      Printf.sprintf "\tsubl\t%s, %s\n" (pp_id_or_imm z') x
  | NonTail (x), Ld (y, V (z), i) -> Printf.sprintf "\tmovl\t(%s,%s,%d), %s\n" y z i x
  | NonTail (x), Ld (y, C (j), i) -> Printf.sprintf "\tmovl\t%d(%s), %s\n" (j * i) y x
  | NonTail (_), St (x, y, V (z), i) -> Printf.sprintf "\tmovl\t%s, (%s,%s,%d)\n" x y z i
  | NonTail (_), St (x, y, C (j), i) -> Printf.sprintf "\tmovl\t%s, %d(%s)\n" x (j * i) y
  | NonTail (x), FMovD (y) ->
    if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x else String.empty
  | NonTail(x), FNegD(y) ->
    (if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x else String.empty) ^
    Printf.sprintf "\txorpd\tmin_caml_fnegd, %s\n" x
  | NonTail(x), FAddD(y, z) ->
    if x = z then
      Printf.sprintf "\taddsd\t%s, %s\n" y x
    else
      (if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x else String.empty) ^
      Printf.sprintf "\taddsd\t%s, %s\n" z x
  | NonTail(x), FSubD(y, z) ->
    if x = z then (* [XXX] ugly *)
      let ss = stacksize () in
      Printf.sprintf "\tmovsd\t%s, %d(%s)\n" z ss reg_sp ^
      (if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x else String.empty) ^
      Printf.sprintf "\tsubsd\t%d(%s), %s\n" ss reg_sp x
    else
      (if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x else String.empty) ^
      Printf.sprintf "\tsubsd\t%s, %s\n" z x
  | NonTail(x), FMulD(y, z) ->
    if x = z then
      Printf.sprintf "\tmulsd\t%s, %s\n" y x
    else
      (if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x else String.empty) ^
      Printf.sprintf "\tmulsd\t%s, %s\n" z x
  | NonTail(x), FDivD(y, z) ->
    if x = z then
      let ss = stacksize () in
      Printf.sprintf "\tmovsd\t%s, %d(%s)\n" z ss reg_sp ^
      (if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x else String.empty) ^
      Printf.sprintf "\tdivsd\t%d(%s), %s\n" ss reg_sp x
    else
      (if x <> y then Printf.sprintf "\tmovsd\t%s, %s\n" y x else String.empty) ^
      Printf.sprintf "\tdivsd\t%s, %s\n" z x
  | NonTail(x), LdDF(y, V(z), i) -> Printf.sprintf "\tmovsd\t(%s,%s,%d), %s\n" y z i x
  | NonTail(x), LdDF(y, C(j), i) -> Printf.sprintf "\tmovsd\t%d(%s), %s\n" (j * i) y x
  | NonTail(_), StDF(x, y, V(z), i) -> Printf.sprintf "\tmovsd\t%s, (%s,%s,%d)\n" x y z i
  | NonTail(_), StDF(x, y, C(j), i) -> Printf.sprintf "\tmovsd\t%s, %d(%s)\n" x (j * i) y
  | NonTail(_), Comment(s) -> Printf.sprintf "\t# %s\n" s
  | NonTail(_), Save(x, y) when List.mem x allregs && not (S.mem y !stackset) ->
    save y;
    Printf.sprintf "\tmovl\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) when List.mem x allfregs && not (S.mem y !stackset) ->
    savef y;
    Printf.sprintf "\tmovsd\t%s, %d(%s)\n" x (offset y) reg_sp
  | NonTail(_), Save(x, y) -> assert (S.mem y !stackset); String.empty
  | NonTail(x), Restore(y) when List.mem x allregs ->
    Printf.sprintf "\tmovl\t%d(%s), %s\n" (offset y) reg_sp x
  | NonTail(x), Restore(y) ->
    assert (List.mem x allfregs);
    Printf.sprintf "\tmovsd\t%d(%s), %s\n" (offset y) reg_sp x
  (* 末尾だったら計算結果を第一レジスタにセットしてret (caml2html: emit_tailret) *)
  | Tail, (Nop | St _ | StDF _ | Comment _ | Save _ as exp) ->
    create_asm' (NonTail (Id.gentmp Type.Unit), exp) ^
    Printf.sprintf "\tret\n";
  | Tail, (Set _ | SetL _ | Mov _ | Neg _ | Add _ | Sub _ | Ld _ as exp) ->
    create_asm' (NonTail(regs.(0)), exp) ^
    Printf.sprintf "\tret\n"
  | Tail, (FMovD _ | FNegD _ | FAddD _ | FSubD _ | FMulD _ | FDivD _ | LdDF _  as exp) ->
    create_asm' (NonTail(fregs.(0)), exp) ^
    Printf.sprintf "\tret\n";
  | Tail, (Restore(x) as exp) ->
    (match locate x with
     | [i] -> create_asm' (NonTail(regs.(0)), exp)
     | [i; j] when i + 1 = j -> create_asm' (NonTail(fregs.(0)), exp)
     | _ -> assert false) ^
    Printf.sprintf "\tret\n"
  | Tail, IfEq(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_tail_if e1 e2 "je" "jne"
  | Tail, IfLE(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_tail_if e1 e2 "jle" "jg"
  | Tail, IfGE(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_tail_if e1 e2 "jge" "jl"
  | Tail, IfFEq(x, y, e1, e2) ->
    (Printf.sprintf "\tcomisd\t%s, %s\n" y x) ^
    create_asm'_tail_if e1 e2 "je" "jne"
  | Tail, IfFLE(x, y, e1, e2) ->
    (Printf.sprintf "\tcomisd\t%s, %s\n" y x) ^
    create_asm'_tail_if e1 e2 "jbe" "ja"
  | NonTail(z), IfEq(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_non_tail_if (NonTail(z)) e1 e2 "je" "jne"
  | NonTail(z), IfLE(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_non_tail_if (NonTail(z)) e1 e2 "jle" "jg"
  | NonTail(z), IfGE(x, y', e1, e2) ->
    (Printf.sprintf "\tcmpl\t%s, %s\n" (pp_id_or_imm y') x) ^
    create_asm'_non_tail_if (NonTail(z)) e1 e2 "jge" "jl"
  | NonTail(z), IfFEq(x, y, e1, e2) ->
    (Printf.sprintf "\tcomisd\t%s, %s\n" y x) ^
    create_asm'_non_tail_if (NonTail(z)) e1 e2 "je" "jne"
  | NonTail(z), IfFLE(x, y, e1, e2) ->
    (Printf.sprintf "\tcomisd\t%s, %s\n" y x) ^
    create_asm'_non_tail_if (NonTail(z)) e1 e2 "jbe" "ja"
  | Tail, CallCls(x, ys, zs) -> (* 末尾呼び出し (caml2html: emit_tailcall) *)
    create_asm'_args [(x, reg_cl)] ys zs ^
    Printf.sprintf "\tjmp\t*(%s)\n" reg_cl
  | Tail, CallDir(Id.L(x), ys, zs) -> (* 末尾呼び出し *)
    create_asm'_args [] ys zs ^
    Printf.sprintf "\tjmp\t%s\n" x;
  | NonTail(a), CallCls(x, ys, zs) ->
    create_asm'_args [(x, reg_cl)] ys zs ^
    let ss = stacksize () in
    (if ss > 0 then Printf.sprintf "\taddl\t$%d, %s\n" ss reg_sp else String.empty) ^
    Printf.sprintf "\tcall\t*(%s)\n" reg_cl ^
    (if ss > 0 then Printf.sprintf "\tsubl\t$%d, %s\n" ss reg_sp else String.empty) ^
    if List.mem a allregs && a <> regs.(0) then
      Printf.sprintf "\tmovl\t%s, %s\n" regs.(0) a
    else if List.mem a allfregs && a <> fregs.(0) then
      Printf.sprintf "\tmovsd\t%s, %s\n" fregs.(0) a
    else
      String.empty
  | NonTail(a), CallDir(Id.L(x), ys, zs) ->
    let ss = stacksize () in
    create_asm'_args [] ys zs ^
    (if ss > 0 then Printf.sprintf "\taddl\t$%d, %s\n" ss reg_sp else String.empty) ^
    Printf.sprintf "\tcall\t%s\n" x ^
    (if ss > 0 then Printf.sprintf "\tsubl\t$%d, %s\n" ss reg_sp else String.empty) ^
    if List.mem a allregs && a <> regs.(0) then
      Printf.sprintf "\tmovl\t%s, %s\n" regs.(0) a
    else if List.mem a allfregs && a <> fregs.(0) then
      Printf.sprintf "\tmovsd\t%s, %s\n" fregs.(0) a
    else
      String.empty
and create_asm'_tail_if e1 e2 b bn =
  let res = ref String.empty in
  let b_else = Id.genid (b ^ "_else") in
  res := !res ^ Printf.sprintf "\t%s\t%s\n" bn b_else;
  let stackset_back = !stackset in
  res := !res ^ create_asm (Tail, e1) ^ Printf.sprintf "%s:\n" b_else;
  stackset := stackset_back;
  !res ^ create_asm (Tail, e2)
and create_asm'_non_tail_if dest e1 e2 b bn =
  let res = ref String.empty in
  let b_else = Id.genid (b ^ "_else") in
  let b_cont = Id.genid (b ^ "_cont") in
  res := !res ^ Printf.sprintf "\t%s\t%s\n" bn b_else;
  let stackset_back = !stackset in
  res := !res ^ create_asm (dest, e1);
  let stackset1 = !stackset in
  res := !res ^ Printf.sprintf "\tjmp\t%s\n" b_cont ^ Printf.sprintf "%s:\n" b_else;
  stackset := stackset_back;
  res := !res ^ create_asm (dest, e2) ^ Printf.sprintf "%s:\n" b_cont;
  let stackset2 = !stackset in
  stackset := S.inter stackset1 stackset2;
  !res
and create_asm'_args x_reg_cl ys zs =
  let res = ref String.empty in
  assert (List.length ys <= Array.length regs - List.length x_reg_cl);
  assert (List.length zs <= Array.length fregs);
  let sw = Printf.sprintf "%d(%s)" (stacksize ()) reg_sp in
  let (i, yrs) =
    List.fold_left
      (fun (i, yrs) y -> (i + 1, (y, regs.(i)) :: yrs))
      (0, x_reg_cl)
      ys
  in
  res :=
    List.fold_left
      (fun s (y, r) -> s ^ Printf.sprintf "\tmovl\t%s, %s\n" y r)
      String.empty
      (shuffle sw yrs);
  let (d, zfrs) =
    List.fold_left
      (fun (d, zfrs) z -> (d + 1, (z, fregs.(d)) :: zfrs))
      (0, [])
      zs
  in
  !res ^
  List.fold_left
    (fun s (z, fr) -> s ^ Printf.sprintf "\tmovsd\t%s, %s\n" z fr)
    String.empty
    (shuffle sw zfrs)

let emit_asm fundef fname =
  Format.printf "compiling %s...\n" fname;
  fundef
  |> RegAlloc.h
  |> Emit.h (Core.Out_channel.create (fname ^ ".s"))

let emit_trace ~fundef ~fname ~iname =
  let { name; body } = fundef |> RegAlloc.h in
  let Id.L (x) = name in
  let oc = Core.Out_channel.create (fname ^ ".s") in
  Printf.fprintf oc ".globl %s\n" x;
  Printf.fprintf oc "%s:\n" x;
  Printf.fprintf oc "\tpushl\t%%ebx\n";
  Printf.fprintf oc ".globl %s1\n" x;
  Printf.fprintf oc "%s1:\n" x;
  g oc (Tail, body);
  Printf.fprintf oc ".globl %s2\n" x;
  Printf.fprintf oc "%s2:\n" x;
  Printf.fprintf oc "\tpopl\t%%eax\n";
  Printf.fprintf oc "jmp\t%s" iname

let emit_trace' ~fundef ~fname ~inameo ~inamen =
  let { name; body } = fundef |> RegAlloc.h in
  let Id.L (x) = name in
  let s =
    Printf.sprintf ".globl %s\n" x
    ^ Printf.sprintf "%s:\n" x
    ^ Printf.sprintf "\tpushl\t%%ebx\n"
    ^ Printf.sprintf ".globl %s1\n" x
    ^ Printf.sprintf "%s1:\n" x
    ^ (create_asm (Tail, body)
       |> replace "\tjmp\tmin_caml_test_trace" "\tjmp\tmin_caml_test_trace1")
    ^ Printf.sprintf ".globl %s2\n" x
    ^ Printf.sprintf "%s2:\n" x
    ^ Printf.sprintf "\tpopl\t%%eax\n"
    ^ Printf.sprintf "\tjmp\t%s" inamen
  in
  let s' = replace inameo (Format.sprintf "%s2" x) s in
  print_newline ();
  print_endline s';
  let oc = open_out (fname ^ ".s") in
  Printf.fprintf oc "%s" s';
  close_out oc