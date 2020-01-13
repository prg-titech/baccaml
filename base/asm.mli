type id_or_imm =
  | V of Id.t
  | C of int

type t =
  (* 命令の列 (caml2html: sparcasm_t) *)
  | Ans of exp
  | Let of (Id.t * Type.t) * exp * t

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
  | Restore of Id.t

(* スタック変数から値を復元 (caml2html: sparcasm_restore) *)

val show : t -> string
val show_exp : exp -> string

type fundef =
  { name : Id.l
  ; args : Id.t list
  ; fargs : Id.t list
  ; body : t
  ; ret : Type.t
  }

val create_fundef
  :  name:Id.l
  -> args:Id.t list
  -> fargs:Id.t list
  -> body:t
  -> ret:Type.t
  -> fundef

type prog =
  | Prog of (Id.l * float) list * (string * Id.t) list * fundef list * t

val string_of_id_or_imm : id_or_imm -> string
val print_id_or_imm : id_or_imm -> unit
val print_t : t -> unit
val print_exp : exp -> unit
val print_fundef : fundef -> unit
val print_prog : prog -> unit
val fletd : Id.t * exp * t -> t
val seq : exp * t -> t
val reg_hp : string
val reg_sp : string
val regs : string array
val fregs : string array
val reg_cl : string
val allregs : string list
val allfregs : string list
val is_reg : string -> bool
val fv : t -> Id.t list
val concat : t -> Id.t * Type.t -> t -> t
val align : int -> int
