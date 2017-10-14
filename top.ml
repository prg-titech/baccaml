open Main

let emit_virtual str =
  Id.counter := 0;
  Typing.extenv := M.empty;
  Lexing.from_string str
  |> Parser.exp Lexer.token
  |> Typing.f
  |> KNormal.f
  |> iter !limit
  |> Alpha.f
  |> Closure.f
  |> Virtual.f
  |> EmitVirtual.f stdout

let str_to_virtual str =
  Id.counter := 0;
  Typing.extenv := M.empty;
  Lexing.from_string str
  |> Parser.exp Lexer.token
  |> Typing.f
  |> KNormal.f
  |> iter !limit
  |> Alpha.f
  |> Closure.f
  |> Virtual.f

let str_to_interp s = interp (Lexing.from_string s)

let str_to_prog_interp s = EmitVirtual.g stdout (str_to_virtual s)
