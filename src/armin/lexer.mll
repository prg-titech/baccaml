{
open Parser
open Lexing
exception Error of string
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
      
    }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alnum = digit | lower | upper

rule token = parse
  | '\\'         { BACKSLASH }
  | '.'          { DOT }
  | ','          { COMMA }
  | '['          { LBRAC }
  | ']'          { RBRAC }
  | '('          { LPAREN }
  | ')'          { RPAREN }
  | '*'          { STAR }
  | '+'          { PLUS }
  | '-'          { MINUS }
  | '='          { EQUAL }
  | '<'          { LT }
  | '>'          { GT }
  | "<="         { LE }
  | ">="         { GE }
  | "true"       { TRUE }
  | "false"      { FALSE }
  | "if"         { IF }
  | "then"       { THEN }
  | "else"       { ELSE }
  | "let"        { LET }
  | "in"         { IN }
  | "rec"        { REC }
  | "fun"        { FUN }
  | digit* { CONST (int_of_string @@ lexeme lexbuf) }
  | alnum* { VAR (lexeme lexbuf) }
  | white { token lexbuf }
  | newline { next_line lexbuf; token lexbuf }
  | eof { EOF }
  | _ { raise @@
        SyntaxError (Printf.sprintf "Illegal string character: %s"
                       (Lexing.lexeme lexbuf))
      }

{

}
