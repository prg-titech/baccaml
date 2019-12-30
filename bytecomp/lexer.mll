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
  | "(*"      { comment lexbuf; token lexbuf }
  | digit+    { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '-'       { MINUS }
  | '+'       { PLUS }
  | '*'       { TIMES }
  | '<'       { LESS }
  | '='       { EQ }
  | ','       { COMMA }
  | '_'       { UNDER_SCORE }
  | "not"     { NOT }
  | "let"     { LET }
  | "rec"     { REC }
  | "%" (lower|upper)+ as it { ANNOT(it) }
  | "in"      { IN }
  | "if"      { IF }
  | "then"    { THEN }
  | "else"    { ELSE }
  | "for"     { FOR }
  | "to"      { TO }
  | "do"      { DO }
  | "done"    { DONE }
  | "Array.make" { ARRAY_MAKE }
  | "->"      { MINUS_GREATER }
  | "<-"      { LESS_MINUS }
  | ";"       { SEMICOLON }
  | '.'       { DOT }
  | eof       { EOF }
  | lower (digit|lower|upper|'_')* { VAR(Lexing.lexeme lexbuf) }
  | _
    { failwith
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }

and comment = parse
  | "*)" { () }
  | "(*" { comment lexbuf; comment lexbuf }
  | eof  { Format.eprintf "warning: unterminated comment@." }
  | _    { comment lexbuf }
