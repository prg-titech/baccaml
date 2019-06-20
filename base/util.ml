let limit = ref 1000

let rec iter n e =
  (* 最適化処理をくりかえす (caml2html: main_iter) *)
  (* Format.eprintf "iteration %d@." n ; *)
  if n = 0 then e
  else
    let e' =
      Beta.f e
      |> Assoc.f
      |> Inline.f
      |> ConstFold.f
      |> Elim.f
    in
    if e = e' then e else iter (n - 1) e'

let virtualize l =
  Id.counter := 0 ;
  Typing.extenv := M.empty ;
  Parser.exp Lexer.token l
  |> Typing.f
  |> KNormal.f
  |> iter !limit
  |> Alpha.f
  |> Closure.f
  |> Virtual.f
  |> Simm.f
