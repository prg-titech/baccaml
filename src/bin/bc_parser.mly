%{

%}

%token <int> INST
%token EOF
%type <Sub.exp list> program
%type <Sub.exp> exp
%start program


%%

program:
  | exp* EOF { $1 }

exp:
  | INST { Sub.Inst $1 }
