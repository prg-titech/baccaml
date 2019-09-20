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
%token MAIN
%token EOF
%left PLUS MINUS
%left TIMES DIV

%type <Syntax.exp> exp
%start exp

%%

simple_exp:
    | LPAREN exp RPAREN     { $2 }
    | INT                   { Int ($1) }
    | VAR                   { Var ($1) }

exp:
    | simple_exp               { $1 }
    | exp PLUS exp             { Add ($1, $3) }
    | exp TIMES exp            { Mul ($1, $3) }
    | exp LESS exp             { LT ($1, $3) }
    | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
    | LET VAR EQ exp IN exp    { Let ($2, $4, $6) }
    | LET REC fundef IN exp    { LetRec ($3, $5) }
    | LET LPAREN RPAREN EQ exp { Main ($5) }
    | VAR actual_args          { Call ($1, $2) }

fundef:
    | VAR formal_args EQ exp   { { name = $1; args = $2; body = $4 } }

formal_args:
    | VAR formal_args { $1 :: $2 }
    | VAR { [$1] }

actual_args:
    | actual_args simple_exp    { $1 @ [$2] }
    | simple_exp                { [$1] }
