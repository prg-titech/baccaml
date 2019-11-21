(* instruction set: a stack machine *)
type inst =
  | UNIT
  | ADD                       (* n2::n1::s -> (n1+n2)::s *)
  | SUB                       (* n2::n1::s -> (n1-n2)::s *)
  | MUL                       (* n2::n1::s -> (n1*n2)::s *)
  | LT                        (* n2::n1::s -> (n1<n2)::s *)
  | CONST (* n *)             (* s         -> n::s *)
  | JUMP_IF_ZERO (* addr *)   (* n::s      -> s *)
  | CALL (* fun-id *)         (* nm::...::n1::s -> pc::nm::...::n1::s *)
  | RET (* n *)               (* r::pc::v1...::vn::s -> r::s  *)
  | DUP (* n *)               (* v1::...::vn::s -> vn::v1::...::vn::s *)
  | HALT                      (* n -> . *)

  (* The next instruction assumes that the stack has (1) o values as
     the parameter to this frame, (2) the return address from this
     frame, (3) l values as the local variables in this frame, and
     (4) n values as the new parameter, and deletes (1) and (3),
     moves (2) to the top of the stack.

     before:
     (stack top) [n new args][l local vars][ret][o old args]...(bottom)
     after:
                               (stack top) [ret][n new args]...(bottom)
  *)

  | FRAME_RESET (* o l n *)
  | POP1                      (* n2::n1::s ->  n2::s *)
  | JUMP (* addr *)
  | METHOD_ENTRY
  | EQ
  | ARRAY_MAKE
  | GET
  | PUT
  (* the following constructors do not represent instructions but
     are defined for expressing operands of some instructions as
     well as label declarations and references *)
  | Literal of int
  | Lref of string
  | Ldef of string

val max_stack_depth : int

val has_args : (inst * bool) list

type value = VInt of int | VArray of int array
type stack = int * value array
val interp : int array -> int -> stack -> value

val int_of_inst : inst -> int
val string_of : inst -> string (* for debugging *)

val show_inst : inst -> string (* for debugging *)

type fundef_bin_t = int array
type fundef_asm_t = inst array
val run_bin : fundef_bin_t -> int
val run_asm : fundef_asm_t -> int

module Value : sig
  val int_of_value : value -> int
  val value_of_int : int -> value
  val value_of_array : int array -> value
end
