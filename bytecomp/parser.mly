%{
  open Syntax

  let annot_of_string str =
    if not (String.contains str '%') then
      failwith "invalid annotation"
    else
      let annot_body = List.tl (String.split_on_char '%' str) in
      if List.length annot_body > 1 then
        failwith "invalid annotation"
      else
        (match List.hd annot_body with
         | "mj" -> Some MethodComp
         | "tj" -> Some TracingComp
         | _ -> None)
%}

%token <int> INT
%token <string> VAR
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token IF THEN ELSE
%token LESS GREATER
%token LESS_EQ GREATER_EQ
%token LET REC
%token <string> ANNOT
%token IN
%token EQ
%token NOT
%token COMMA
%token ARRAY_MAKE
%token LESS_MINUS
%token MINUS_GREATER
%token SEMICOLON
%token DOT
%token FOR TO DO DONE
%token MAIN
%token EOF

%left COMMA
%left PLUS MINUS
%left TIMES DIV
%left prec_app
%nonassoc UMINUS        /* highest precedence */
%right prec_let
%right SEMICOLON

%type <Syntax.exp> exp
%start exp

%%

simple_exp:
    | LPAREN RPAREN          { Unit }
    | LPAREN exp RPAREN      { $2 }
    | INT                    { Int ($1) }
    | VAR                    { Var ($1) }
    | NOT exp                { Not ($2) }
    | simple_exp DOT LPAREN exp RPAREN { Get ($1, $4) }

exp:
    | simple_exp               { $1 }
    | exp PLUS exp             { Add ($1, $3) }
    | exp MINUS exp            { Sub ($1, $3) }
    | exp TIMES exp            { Mul ($1, $3) }
    | exp LESS exp             { LT ($1, $3) }
    | exp EQ exp               { Eq ($1, $3) }
    | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
    | LET VAR EQ exp IN exp    %prec prec_let { Let ($2, $4, $6) }
    | fundef IN exp            %prec prec_let { LetRec ($1, $3) }
    | LET LPAREN RPAREN EQ exp %prec prec_let { LetRec ({name="main"; args=[]; body=$5; annot=None}, Unit) }
    | VAR actual_args          %prec prec_app { Call (None, $1, $2) }
    | exp SEMICOLON exp        { Let (Id.gentmp (), $1, $3) }
    | ARRAY_MAKE simple_exp simple_exp       { Array($2, $3) }
    | simple_exp DOT LPAREN exp RPAREN LESS_MINUS simple_exp SEMICOLON exp { Put ($1, $4, $7, $9) }
    | FOR VAR EQ exp TO exp DO exp DONE SEMICOLON exp { For(Range($2, $4, $6), $8, $11) }
    | error
        { failwith
            (Printf.sprintf "parse error near characters %d-%d"
               (Parsing.symbol_start ())
               (Parsing.symbol_end ()))  }

fundef:
    | LET REC VAR formal_args EQ exp   { { name=$3; args=$4; body=$6; annot=None } }
    | LET ANNOT REC VAR formal_args EQ exp { { name=$4; args=$5; body=$7; annot=annot_of_string $2 } }

formal_args:
    | VAR formal_args { $1 :: $2 }
    | VAR { [$1] }

actual_args:
    | actual_args simple_exp    { $1 @ [$2] }
    | simple_exp                { [$1] }
