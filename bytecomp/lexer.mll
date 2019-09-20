{
  open Parser
  exception Eof
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
    space+    { token lexbuf }
  | '('       { LPAREN }
  | ')'       { RPAREN }
  | digit+    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '-'       { MINUS }
  | '+'       { PLUS }
  | '*'       { TIMES }
  | '<'       { LESS }
  | '='       { EQ }
  | ','       { COMMA }
  | "let"     { LET }
  | "rec"     { REC }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | eof       { EOF }
  | lower (digit|lower|upper|'_')* { VAR(Lexing.lexeme lexbuf) }
  | _
    { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }
