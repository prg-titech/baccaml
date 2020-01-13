(* 2オペランドではなく3オペランドのx86アセンブリもどき *)
type id_or_imm =
  | V of Id.t
  | C of int
[@@deriving show]

let string_of_id_or_imm = function
  | V id -> "V " ^ id
  | C n -> "C " ^ string_of_int n
;;

let rec print_id_or_imm = function
  | V id_t ->
    print_string "V (";
    print_string id_t;
    print_string ")"
  | C n ->
    print_string "C ";
    print_int n;
    print_string " "
;;

type t =
  (* 命令の列 (caml2html: sparcasm_t) *)
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t
[@@deriving show]

and exp =
  (* 一つ一つの命令に対応する式 (caml2html: sparcasm_exp) *)
  | Nop
  | Set of int
  | SetL of Id.l
  | Mov of Id.t
  | SMov of string
  | Neg of Id.t
  | Add of Id.t * id_or_imm
  | Sub of Id.t * id_or_imm
  | Mul of Id.t * id_or_imm
  | Div of Id.t * id_or_imm
  | Mod of Id.t * id_or_imm
  | Ld of Id.t * id_or_imm * int
  | St of Id.t * Id.t * id_or_imm * int
  | FMovD of Id.t
  | FNegD of Id.t
  | FAddD of Id.t * Id.t
  | FSubD of Id.t * Id.t
  | FMulD of Id.t * Id.t
  | FDivD of Id.t * Id.t
  | LdDF of Id.t * id_or_imm * int
  | StDF of Id.t * Id.t * id_or_imm * int
  | Comment of string
  (* virtual instructions *)
  | IfEq of Id.t * id_or_imm * t * t
  | IfLE of Id.t * id_or_imm * t * t
  | IfGE of Id.t * id_or_imm * t * t (* 左右対称ではないので必要 *)
  | SIfEq of Id.t * id_or_imm * t * t
  | SIfLE of Id.t * id_or_imm * t * t
  | SIfGE of Id.t * id_or_imm * t * t (* 左右対称ではないので必要 *)
  | IfFEq of Id.t * Id.t * t * t
  | IfFLE of Id.t * Id.t * t * t
  | SIfFEq of Id.t * Id.t * t * t
  | SIfFLE of Id.t * Id.t * t * t
  (* closure address, integer arguments, and float arguments *)
  | CallCls of Id.t * Id.t list * Id.t list
  | CallDir of Id.l * Id.t list * Id.t list
  | Save of Id.t * Id.t (* レジスタ変数の値をスタック変数へ保存 (caml2html: sparcasm_save) *)
  | Restore of Id.t (* スタック変数から値を復元 (caml2html: sparcasm_restore) *)
[@@deriving show]

let print_tab () = print_string "  "
let print_semi_colon () = print_string ","

let rec print_t = function
  | Ans exp ->
    print_string "Ans (";
    print_exp exp;
    print_string ")"
  | Let ((id, typ), exp, t) ->
    print_string "Let (";
    print_string "(";
    print_string id;
    print_string ", ";
    Type.print_type typ;
    print_string ")";
    print_string ",";
    print_tab ();
    print_exp exp;
    print_string ", ";
    print_newline ();
    print_tab ();
    print_t t;
    print_string ")"

and print_exp = function
  | Nop -> print_string "Nop"
  | Set n ->
    print_string "Set (";
    print_int n;
    print_string ")"
  | SetL id_l ->
    print_string "SetL (";
    Id.print_id_l id_l;
    print_string ")"
  | Mov id ->
    print_string "Mov (";
    print_string id;
    print_string ")"
  | SMov id ->
    print_string "SMov (";
    print_string id;
    print_string ")"
  | Neg id ->
    print_string "Neg (";
    print_string id;
    print_string ")"
  | Add (x, y) ->
    print_string "Add (";
    print_string x;
    print_semi_colon ();
    print_id_or_imm y;
    print_string ")"
  | Sub (x, y) ->
    print_string "Sub (";
    print_string x;
    print_semi_colon ();
    print_id_or_imm y;
    print_string ")"
  | Mul (x, y) ->
    print_string "Mul (";
    print_string x;
    print_semi_colon ();
    print_id_or_imm y;
    print_string ")"
  | Ld (x, y, n) ->
    print_string "Ld (";
    print_string x;
    print_semi_colon ();
    print_id_or_imm y;
    print_semi_colon ();
    print_int n;
    print_string ")"
  | St (x, y, z, n) ->
    print_string "St (";
    print_string x;
    print_semi_colon ();
    print_string y;
    print_semi_colon ();
    print_id_or_imm z;
    print_semi_colon ();
    print_int n;
    print_string ")"
  | IfEq (x, y, t1, t2) ->
    print_string "IfEq (";
    print_string x;
    print_semi_colon ();
    print_id_or_imm y;
    print_semi_colon ();
    print_newline ();
    print_tab ();
    print_t t1;
    print_newline ();
    print_tab ();
    print_t t2;
    print_string ")"
  | IfLE (x, y, t1, t2) ->
    print_string "IfLE (";
    print_string x;
    print_semi_colon ();
    print_id_or_imm y;
    print_semi_colon ();
    print_newline ();
    print_tab ();
    print_t t1;
    print_newline ();
    print_tab ();
    print_t t2;
    print_string ")"
  | IfGE (x, y, t1, t2) ->
    print_string "IfGE (";
    print_string x;
    print_semi_colon ();
    print_id_or_imm y;
    print_semi_colon ();
    print_newline ();
    print_tab ();
    print_t t1;
    print_newline ();
    print_tab ();
    print_t t2;
    print_string ")"
  | SIfEq (x, y, t1, t2) ->
    print_string "SIfEq (";
    print_string x;
    print_semi_colon ();
    print_id_or_imm y;
    print_semi_colon ();
    print_newline ();
    print_tab ();
    print_t t1;
    print_newline ();
    print_tab ();
    print_t t2;
    print_string ")"
  | SIfLE (x, y, t1, t2) ->
    print_string "SIfLE (";
    print_string x;
    print_semi_colon ();
    print_id_or_imm y;
    print_semi_colon ();
    print_newline ();
    print_tab ();
    print_t t1;
    print_newline ();
    print_tab ();
    print_t t2;
    print_string ")"
  | SIfGE (x, y, t1, t2) ->
    print_string "SIfGE (";
    print_string x;
    print_semi_colon ();
    print_id_or_imm y;
    print_semi_colon ();
    print_newline ();
    print_tab ();
    print_t t1;
    print_newline ();
    print_tab ();
    print_t t2;
    print_string ")"
  | CallCls (x, ys, zs) ->
    print_string "CallCls (";
    print_string x;
    print_semi_colon ();
    print_string "[";
    ys
    |> List.iter (fun y ->
           print_string y;
           print_string "; ");
    print_string "]";
    print_string ",";
    print_string "[";
    zs
    |> List.iter (fun z ->
           print_string z;
           print_string "; ");
    print_string "]";
    print_string ")"
  | CallDir (x, ys, zs) ->
    print_string "CallDir (";
    Id.print_id_l x;
    print_semi_colon ();
    print_string "[";
    ys
    |> List.iter (fun y ->
           print_string y;
           print_string "; ");
    print_string "]";
    print_semi_colon ();
    print_string "[";
    zs
    |> List.iter (fun z ->
           print_string z;
           print_string "; ");
    print_string "]";
    print_string ")"
  | _ -> ()
;;

type fundef =
  { name : Id.l
  ; args : Id.t list
  ; fargs : Id.t list
  ; body : t
  ; ret : Type.t
  }

let create_fundef ~name ~args ~fargs ~body ~ret =
  { name; args; fargs; body; ret }
;;

let print_fundef { name; args; fargs; body; ret } =
  print_string "{ ";
  print_string "name= ";
  Id.print_id_l name;
  print_string "; ";
  print_string "args= ";
  print_string "[";
  args
  |> List.iter (fun arg ->
         print_string arg;
         print_string "; ");
  print_string "]";
  print_string "; ";
  print_string "fargs= ";
  print_string "[";
  fargs
  |> List.iter (fun farg ->
         print_string farg;
         print_string "; ");
  print_string "]";
  print_string "; ";
  print_string "body= ";
  print_newline ();
  print_tab ();
  print_t body;
  print_newline ();
  print_tab ();
  print_string "ret= ";
  Type.print_type ret;
  print_string ";";
  print_newline ();
  print_string "}";
  print_newline ()
;;

(* プログラム全体 = 浮動小数点数テーブル + トップレベル関数 + メインの式 (caml2html: sparcasm_prog) *)
type prog =
  | Prog of (Id.l * float) list * (string * Id.t) list * fundef list * t

let print_prog (Prog (tbl, const, fundefs, t)) =
  print_string "const table:\n";
  print_tab ();
  print_string "[";
  List.iter (fun (x, y) -> Printf.printf "(%s, %s), " x y) const;
  print_string "]\n";
  print_string "Prog (";
  print_string "[";
  print_string "]";
  print_semi_colon ();
  fundefs
  |> List.iter (fun fundef ->
         print_fundef fundef;
         print_semi_colon ();
         print_newline ());
  print_string "main=\n";
  print_tab ();
  print_t t
;;

let fletd (x, e1, e2) = Let ((x, Type.Float), e1, e2)
let seq (e1, e2) = Let ((Id.gentmp Type.Unit, Type.Unit), e1, e2)

let regs =
  (* Array.init 16 (fun i -> Printf.sprintf "%%r%d" i) *)
  [| "%eax"; "%ebx"; "%ecx"; "%edx"; "%esi"; "%edi" |]
;;

let fregs = Array.init 8 (fun i -> Printf.sprintf "%%xmm%d" i)
let allregs = Array.to_list regs
let allfregs = Array.to_list fregs
let reg_cl = regs.(Array.length regs - 1)

(* closure address (caml2html: sparcasm_regcl) *)

(* let reg_sw = regs.(Array.length regs - 1) (* temporary for swap *) let
   reg_fsw = fregs.(Array.length fregs - 1) (* temporary for swap *) *)
let reg_sp = "%ebp" (* stack pointer *)

let reg_hp = "min_caml_hp" (* heap pointer (caml2html: sparcasm_reghp) *)

(* let reg_ra = "%eax" (* return address *) *)
let is_reg x = x.[0] = '%' || x = reg_hp

(* super-tenuki *)
let rec remove_and_uniq xs = function
  | [] -> []
  | x :: ys when S.mem x xs -> remove_and_uniq xs ys
  | x :: ys -> x :: remove_and_uniq (S.add x xs) ys
;;

(* free variables in the order of use (for spilling) (caml2html: sparcasm_fv) *)
let fv_id_or_imm = function V x -> [ x ] | _ -> []

let rec fv_exp = function
  | Nop | Set _ | SetL _ | Comment _ | Restore _ | SMov _ -> []
  | Mov x | Neg x | FMovD x | FNegD x | Save (x, _) -> [ x ]
  | Add (x, y') | Sub (x, y') | Mul (x, y') | Div (x, y') | Mod (x, y') | Ld (x, y', _) | LdDF (x, y', _) ->
    x :: fv_id_or_imm y'
  | St (x, y, z', _) | StDF (x, y, z', _) -> x :: y :: fv_id_or_imm z'
  | FAddD (x, y) | FSubD (x, y) | FMulD (x, y) | FDivD (x, y) -> [ x; y ]
  | IfEq (x, y', e1, e2)
  | IfLE (x, y', e1, e2)
  | IfGE (x, y', e1, e2)
  | SIfEq (x, y', e1, e2)
  | SIfLE (x, y', e1, e2)
  | SIfGE (x, y', e1, e2) ->
    (x :: fv_id_or_imm y') @ remove_and_uniq S.empty (fv e1 @ fv e2)
  (* uniq here just for efficiency *)
  | IfFEq (x, y, e1, e2)
  | IfFLE (x, y, e1, e2)
  | SIfFEq (x, y, e1, e2)
  | SIfFLE (x, y, e1, e2) ->
    x :: y :: remove_and_uniq S.empty (fv e1 @ fv e2)
  (* uniq here just for efficiency *)
  | CallCls (x, ys, zs) -> (x :: ys) @ zs
  | CallDir (_, ys, zs) -> ys @ zs

and fv = function
  | Ans exp -> fv_exp exp
  | Let ((x, t), exp, e) -> fv_exp exp @ remove_and_uniq (S.singleton x) (fv e)
;;

let fv e = remove_and_uniq S.empty (fv e)

let rec concat e1 xt e2 =
  match e1 with
  | Ans exp -> Let (xt, exp, e2)
  | Let (yt, exp, e1') -> Let (yt, exp, concat e1' xt e2)
;;

let align i = if i mod 8 = 0 then i else i + 4
