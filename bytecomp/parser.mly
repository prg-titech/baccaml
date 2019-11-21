%{
  open Syntax
%}

%token <int> INT
%token <string> VAR
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token IF THEN ELSE
%token LESS GREATER
%token LESS_EQ GREATER_EQ
%token LET REC
%token IN
%token EQ
%token COMMA
%token ARRAY_MAKE
%token LESS_MINUS
%token MINUS_GREATER
%token SEMICOLON
%token DOT
%token FOR WHILE TO DO DONE
%token MAIN
%token EOF
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS        /* highest precedence */

%type <Syntax.exp> exp
%start exp

%%

simple_exp:
    | LPAREN RPAREN          { Unit }
    | LPAREN exp RPAREN      { $2 }
    | INT                    { Int ($1) }
    | VAR                    { Var ($1) }
    | simple_exp DOT LPAREN exp RPAREN { Get ($1, $4) }

exp:
    | simple_exp               { $1 }
    | exp PLUS exp             { Add ($1, $3) }
    | exp MINUS exp            { Sub ($1, $3) }
    | exp TIMES exp            { Mul ($1, $3) }
    | exp LESS exp             { LT ($1, $3) }
    | exp EQ exp               { Eq ($1, $3) }
    | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
    | LET VAR EQ exp IN exp    { Let ($2, $4, $6) }
    | LET REC fundef IN exp    { LetRec ($3, $5) }
    | LET LPAREN RPAREN EQ exp { LetRec ({name="main"; args=[]; body=$5}, Unit) }
    | VAR actual_args          { Call ($1, $2) }
    | exp SEMICOLON exp        { Let (Id.gentmp (), $1, $3) }
    | ARRAY_MAKE exp exp       { Array ($2, $3) }
    | simple_exp DOT LPAREN exp RPAREN LESS_MINUS simple_exp SEMICOLON exp { Put ($1, $4, $7, $9) }
    | FOR VAR EQ exp TO exp DO exp DONE SEMICOLON exp { For(Range($2, $4, $6), $8, $11) }
    | WHILE exp DO exp DONE SEMICOLON exp { While($2, $4, $7) }

fundef:
    | VAR formal_args EQ exp   { { name = $1; args = $2; body = $4 } }

formal_args:
    | VAR formal_args { $1 :: $2 }
    | VAR { [$1] }

actual_args:
    | actual_args simple_exp    { $1 @ [$2] }
    | simple_exp                { [$1] }
