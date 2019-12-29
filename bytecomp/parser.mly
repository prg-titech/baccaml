%{
  open Syntax
  open Parsing

  exception Parse_failure of string

  let annot_of_string str =
    if not (String.contains str '%') then
      failwith "invalid annotation"
    else
      let annot_body = List.tl (String.split_on_char '%' str) in
      (match List.hd annot_body with
         "mj" -> Some MethodComp
       | "tj" -> Some TracingComp
       | _ -> None)

%}

%token <int> INT
%token <float> FLOAT
%token <string> VAR
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token IF THEN ELSE
%token LESS GREATER LESS_GREATER
%token LESS_EQ GREATER_EQ
%token LET REC
%token <string> ANNOT
%token IN
%token EQ
%token NOT
%token COMMA
%token ARRAY_CREATE
%token LESS_MINUS
%token MINUS_GREATER
%token SEMICOLON SEMISEMI
%token DOT
%token FOR TO DO DONE
%token MAIN
%token EOF
%nonassoc UMINUS        /* highest precedence */

%nonassoc IN
%right prec_let
%right SEMICOLON
%right prec_if
%right LESS_MINUS
%nonassoc prec_tuple
%left COMMA
%left EQ LESS_EQ LESS GREATER LESS_EQ GREATER_EQ
%left PLUS MINUS PLUS_DOT MINUS_DOT
%right prec_unary_minus
%left prec_app
%left DOT

%type <Syntax.exp> exp
%start exp

%%

simple_exp:
| LPAREN exp RPAREN { $2 }
| LPAREN RPAREN { Unit }
| INT { Int($1) }
| FLOAT { Float($1) }
| VAR { Var($1) }
| simple_exp DOT LPAREN exp RPAREN { Get($1, $4) }

exp:
| simple_exp { $1 }
| NOT exp %prec prec_app { Not($2) }
| MINUS exp
    %prec prec_unary_minus
    { match $2 with
    | Float(f) -> Float(-.f)
    | e -> Neg(e) }
| exp PLUS exp  { Add($1, $3) }
| exp MINUS exp { Sub($1, $3) }
| exp TIMES exp { Mul($1, $3) }
| exp EQ exp    { Eq($1, $3) }
| exp LESS_GREATER exp { Not(Eq($1, $3)) }
| exp LESS exp  { Not(LE($3, $1)) }
| exp GREATER exp { Not(LE($1, $3)) }
| exp LESS_EQ exp { LE($1, $3) }
| exp GREATER_EQ exp { LE($3, $1) }
| IF exp THEN exp ELSE exp %prec prec_if { If($2, $4, $6) }

/*
| MINUS_DOT exp %prec prec_unary_minus { FNeg($2) }
| exp PLUS_DOT exp  { FAdd($1, $3) }
| exp MINUS_DOT exp { FSub($1, $3) }
| exp AST_DOT exp   { FMul($1, $3) }
| exp SLASH_DOT exp { FDiv($1, $3) }
*/

| LET VAR EQ exp IN exp %prec prec_let { Let($2, $4, $6) }
| LET LPAREN RPAREN EQ exp { LetRec ({name="main"; args=[]; body=$5; annot=None}, Unit) }
| LET REC fundef SEMISEMI exp %prec prec_let { LetRec($3, $5) }
| LET REC fundef IN exp %prec prec_let { LetRec($3, $5) }
| simple_exp actual_args %prec prec_app { Call(None, $1, $2) }

/*
| elems %prec prec_tuple { Tuple($1) }
| LET LPAREN pat RPAREN EQ exp IN exp { LetTuple($3, $6, $8) }
*/
| simple_exp DOT LPAREN exp RPAREN LESS_MINUS exp { Put($1, $4, $7) }
| exp SEMICOLON exp { Let(Id.gentmp (), $1, $3) }

| ARRAY_CREATE simple_exp simple_exp %prec prec_app { Array($2, $3) }
| error
    { failwith
        (Printf.sprintf "parse error near characters %d-%d"
           (Parsing.symbol_start ())
           (Parsing.symbol_end ())) }

fundef:
| VAR formal_args EQ exp
    { { name = $1; args = $2; body = $4; annot = None } }
| LET ANNOT REC VAR formal_args EQ exp
    { { name=$4; args=$5; body=$7; annot=annot_of_string $2 } }

formal_args:
| VAR formal_args
    { $1 :: $2 }
| VAR
    { [$1] }

actual_args:
| actual_args simple_exp
    %prec prec_app
    { $1 @ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }

elems:
| elems COMMA exp
    { $1 @ [$3] }
| exp COMMA exp
    { [$1; $3] }

pat:
| pat COMMA VAR
    { $1 @ [$3] }
| VAR COMMA VAR
    { [$1; $3] }
