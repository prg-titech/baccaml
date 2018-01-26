open Mincaml_util

let emit_virtual str =
  Id.counter := 0;
  Typing.extenv := M.empty;
  Lexing.from_string str
  |> Parser.exp Lexer.token
  |> Typing.f
  |> KNormal.f
  |> InterpMain.iter !InterpMain.limit
  |> Alpha.f
  |> Closure.f
  |> Virtual.f
  |> Emit_virtual.f stdout

let str_to_virtual str =
  Id.counter := 0;
  Typing.extenv := M.empty;
  Lexing.from_string str
  |> Parser.exp Lexer.token
  |> Typing.f
  |> KNormal.f
  |> InterpMain.iter !InterpMain.limit
  |> Alpha.f
  |> Closure.f
  |> Virtual.f

let str_to_interp s = ignore (InterpMain.interp (Lexing.from_string s))

let str_to_prog_interp s = Emit_virtual.g stdout (str_to_virtual s)
