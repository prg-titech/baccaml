open Main

let string_of_virtual s = virtualize (Lexing.from_string s)

let string_of_interp s = interp (Lexing.from_string s)
