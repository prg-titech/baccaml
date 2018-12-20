%{
  open Sub
%}

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

program:| exp* EOF { $1 }
exp:    | INST { Inst $1 }
        | VAR { Var $1 }
        | GREEN LPAREN v1 = VAR COMMA i1 = INST RPAREN { Green (v1, i1) }
        | RED LPAREN v1 =  VAR COMMA i1 = INST RPAREN { Red (v1, i1) }
        | JIT_TYPE LPAREN v = VAR RPAREN { Jit_type (v) }
        | MERGE_PC LPAREN i = INST RPAREN { Merge_pc (i) }
