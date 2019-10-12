{
  exception UnknownToken of string
  exception SyntaxError of string
  open Parser
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']

rule token = parse
| space+
    { token lexbuf }
| "(*"
    { comment lexbuf;
      token lexbuf }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| "true"
    { BOOL(true) }
| "false"
    { BOOL(false) }
| "not"
    { NOT }
| digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
| digit+
    { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| '"'
    { read_string (Buffer.create 17) lexbuf }
| '-'
    { MINUS }
| '+'
    { PLUS }
| '*'
    { AST }
| "-."
    { MINUS_DOT }
| "+."
    { PLUS_DOT }
| "*."
    { AST_DOT }
| "/."
    { SLASH_DOT }
| '='
    { EQUAL }
| "<>"
    { LESS_GREATER }
| "<="
    { LESS_EQUAL }
| ">="
    { GREATER_EQUAL }
| '<'
    { LESS }
| '>'
    { GREATER }
| "begin"
    { BEGIN }
| "end"
    { END }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| "if"
    { IF }
| "@if"
    { ATIF }
| "then"
    { THEN }
| "else"
    { ELSE }
| "let"
    { LET }
| "in"
    { IN }
| "rec"
    { REC }
| "fun"
    { FUN }
| ";;"
    { SEMISEMI }
| '['
    { LBRAC }
| ']'
    { RBRAC }
| '|'
    { VBAR }
| ','
    { COMMA }
| '_'
    { IDENT(Id.gentmp Type.Unit) }
| "Array.make" | "Array.create" (* [XX] ad hoc *)
    { ARRAY_CREATE }
| '.'
    { DOT }
| "->"
    { MINUS_GREATER }
| "<-"
    { LESS_MINUS }
| ';'
    { SEMICOLON }
| eof
    { EOF }
| lower (digit|lower|upper|'_')*
    { IDENT(Lexing.lexeme lexbuf) }
| _
    { raise (UnknownToken
        (Printf.sprintf "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf))) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }

and read_string buf = parse
| '"'       { STRING (Buffer.contents buf) }
| '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
| '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
| '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
| '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
| [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
| _   { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (SyntaxError ("String is not terminated")) }

{

}
