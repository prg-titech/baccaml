{
open Bc_parser
open Lexing

exception SyntaxError of string
}

let space = ['\t' '\n' '\r' ' ']
let digit = ['0'-'9']


rule token = parse
| space+ { token lexbuf (* use recursion to ignore *) }
| digit+ { INST (int_of_string @@ lexeme lexbuf) }
| eof { EOF }

{

}
