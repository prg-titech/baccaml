%{
    open Syntax
%}

%token LBRAC RBRAC LPAREN RPAREN BACKSLASH DOT COMMA
%token <Syntax.var> VAR
%token <int> CONST
%token PLUS MINUS STAR
%token LET IN
%token REC
%token FUN
%token TRUE FALSE
%token EQUAL
%token GT LT LE GE
%token IF THEN ELSE
%token EOF


%start parse
%type <Syntax.exp option> parse
%%

parse: | e = exp EOF { Some e }
       | EOF         { None }
exp:   | VAR { Var $1 }
       | CONST { Int $1 }
       | exp PLUS exp { Add ($1, $3) }
       | exp MINUS exp { Sub ($1, $3) }
       | exp STAR exp { Mul ($1, $3) }
       | exp LT exp { LT ($1, $3) }
       | IF exp THEN exp ELSE exp { If ($2, $4, $6) }
       | LET VAR EQUAL exp IN exp { Let ($2, $4, $6) }
       | v = VAR LPAREN es = separated_list(COMMA, exp) RPAREN { Call (v, es) }
       | FUN v = VAR LPAREN lst = separated_list(COMMA, VAR) RPAREN EQUAL e1 = exp IN e2 = exp
         { LetRec ({ name = v; args = lst; body = e1 }, e2) }
       | LET REC v = VAR LPAREN lst = separated_list(COMMA, VAR) RPAREN EQUAL e1 = exp IN e2 = exp
         { LetRec ({ name = v; args = lst; body = e1 }, e2) }