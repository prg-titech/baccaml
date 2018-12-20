{
open Bc_parser
open Lexing

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let newline = '\r' | '\n' | "\r\n"
let space = ['\t' '\n' '\r' ' ']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alnum = digit | alpha

rule token = parse
  | '['           { LBRAC  }
  | ']'           { RBRAC  }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | ','           { COMMA }
  | '"'           { QUOT }
  | "Green"       { GREEN }
  | "Red"         { RED }
  | "Merge_pc"    { MERGE_PC }
  | "Jit_type"    { JIT_TYPE }
  | digit*        { INST (int_of_string @@ lexeme lexbuf) }
  | alnum*        { VAR (lexeme lexbuf) }
  | space+        { token lexbuf (* use recursion to ignore *) }
  | newline       { next_line lexbuf; token lexbuf }
  | eof           { EOF }
  | _             { raise (SyntaxError
                               (Printf.sprintf "At offset %d: unexpected character.\n"
                                  (Lexing.lexeme_start lexbuf))) }

{

}
