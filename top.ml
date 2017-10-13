open Main

let str_of_interp s = interp (Lexing.from_string s)

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

let str_of_virtual str =
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
