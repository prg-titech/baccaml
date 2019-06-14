%{
  open Sub
%}

%token EOF
%token COMMA
%token LBRAC
%token RBRAC
%token LPAREN
%token RPAREN
%token QUOT
%token <int> INST
%token <string> VAR
%token GREEN
%token RED
%token JIT_TYPE
%token MERGE_PC
%token EOF
%type <Sub.exp list> program
%type <Sub.exp> exp
%start program


%%

program:| exp EOF { [$1] }
        | exp program { $1 :: $2 }

exp:    | INST { Inst $1 }
        | VAR { Var $1 }
        | GREEN LPAREN VAR COMMA INST RPAREN { Green ($3, $5) }
        | RED LPAREN VAR COMMA INST RPAREN { Red ($3, $5) }
        | JIT_TYPE LPAREN VAR RPAREN { Jit_type ($3) }
        | MERGE_PC LPAREN INST RPAREN { Merge_pc ($3) }
