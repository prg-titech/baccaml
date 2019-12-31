type inst =
  | UNIT
  | ADD
  | SUB
  | MUL
  | LT
  | CONST
  | JUMP_IF_ZERO (* addr *)
  | CALL (* fun-id *)
  | RET
  | DUP (* n *)
  | HALT
  | FRAME_RESET (* o l n *)
  | POP1
  | JUMP
  | METHOD_ENTRY
  | EQ
  | ARRAY_MAKE
  | GET
  | PUT
  | NOT
  | POP0
  | METHOD_COMP                 (* annotation *)
  | CALL_HS
  | DUP0
  | CONST0
  | Literal of int
  | Lref of string
  | Ldef of string
[@@deriving show]

(* the next array determines the opcode *)
let insts = [|
  UNIT;
  ADD;
  SUB;
  MUL;
  LT;
  CONST;
  JUMP_IF_ZERO; (* LOAD; STORE; *)
  CALL;
  RET;
  DUP;
  HALT;
  FRAME_RESET;
  POP1;
  JUMP;
  METHOD_ENTRY;
  EQ;
  ARRAY_MAKE;
  GET;
  PUT;
  NOT;
  POP0;
  METHOD_COMP;
  CALL_HS;
  DUP0;
  CONST0;
|]

let has_args = [
  UNIT, false;
  ADD, false;
  SUB, false;
  MUL, false;
  LT, false;
  CONST, true;
  JUMP_IF_ZERO, true;
  JUMP, true;
  CALL, true;
  RET, true;
  DUP, true;
  HALT, false;
  FRAME_RESET, true;
  POP1, false;
  JUMP, true;
  METHOD_ENTRY, false;
  EQ, false;
  ARRAY_MAKE, false;
  GET, false;
  PUT, false;
  NOT, false;
  POP0, false;
  METHOD_COMP, false;
  CALL_HS, true;
  DUP0, false;
  CONST0, false;
]
