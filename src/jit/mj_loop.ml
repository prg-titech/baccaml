open Core
open Mincaml
open Util
open Asm

let loop_start_l = Id.L ("min_caml_loop_start")
let loop_end_l = Id.L ("min_caml_loop_end")

let rec find_loop_t name = function
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) when id_l = loop_end_l ->
    Ans (CallDir (Id.L (name), args, fargs))
  | Let ((dest, typ), exp, body) -> Let ((dest, typ), exp, find_loop_t name body)
  | Ans (exp) -> find_loop_exp name exp

and find_loop_exp name = function
  | IfEq (id_t, id_or_imm, t1, t2)
  | IfLE (id_t, id_or_imm, t1, t2)
  | IfGE (id_t, id_or_imm, t1, t2) as exp ->
    begin match find_loop_t name t1 with
      | t ->
        begin match exp with
          | IfEq _ ->
            Ans (IfEq (id_t, id_or_imm,
                       t,
                       Ans (CallDir (Id.L ("min_caml_test_trace"), [], []))
                      ))
          | IfLE _ ->
            Ans (IfLE (id_t, id_or_imm,
                       t,
                       Ans (CallDir (Id.L ("min_caml_test_trace"), [], []))
                      ))
          | IfGE _ ->
            Ans (IfGE (id_t, id_or_imm,
                       t,
                       Ans (CallDir (Id.L ("min_caml_test_trace"), [], []))
                      ))
          | _ -> assert false
        end
      | exception Not_found ->
        match find_loop_t name t2 with
        | t ->
          begin match exp with
          | IfEq _ ->
            Ans (IfEq (id_t, id_or_imm,
                       Ans (CallDir (Id.L ("min_caml_test_trace"), [], [])),
                       t
                      ))
          | IfLE _ ->
            Ans (IfLE (id_t, id_or_imm,
                       Ans (CallDir (Id.L ("min_caml_test_trace"), [], [])),
                       t
                      ))
          | IfGE _ ->
            Ans (IfGE (id_t, id_or_imm,
                       Ans (CallDir (Id.L ("min_caml_test_trace"), [], [])),
                       t
                      ))
          | _ -> assert false
        end
        | exception Not_found ->
          Logger.debug "find_loop_exp in if branch is failed.";
          raise Not_found
    end
  | CallDir (id_l, args, fargs) when id_l = loop_end_l ->
    Ans (CallDir (Id.L (name), args, fargs))
  | exp -> raise Not_found

let rec find_loop name = function
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) when id_l = loop_start_l ->
    Logger.debug "find_loop start";
    { name = Id.L (name); args = args; fargs = fargs; body = find_loop_t name body; ret = Type.Int }
  | Let ((dest, typ), CallDir (id_l, args, fargs), body) when id_l = loop_end_l ->
    Logger.debug "find_loop failed.";
    raise Not_found
  | Let ((dest, typ), exp, body) ->
    find_loop name body
  | Ans (exp) ->
    begin match exp with
      | IfEq (id_t, id_or_imm, t1, t2)
      | IfLE (id_t, id_or_imm, t1, t2)
      | IfGE (id_t, id_or_imm, t1, t2) ->
        begin match find_loop name t1 with
          | { name; args; fargs; body; ret } ->
            let body' =
              match exp with
              | IfEq _ ->
                Ans (IfEq (id_t, id_or_imm,
                           body,
                           Ans (CallDir (Id.L ("min_caml_test_trace"), args, fargs))))
              | IfLE _ ->
                Ans (IfLE (id_t, id_or_imm,
                           body,
                           Ans (CallDir (Id.L ("min_caml_test_trace"), args, fargs))))
              | IfGE _ ->
                Ans (IfGE (id_t, id_or_imm,
                           body,
                           Ans (CallDir (Id.L ("min_caml_test_trace"), args, fargs))))
              | _ -> assert false
            in
            { name = name; args = args; fargs = fargs; body = body'; ret = ret }
          | exception Not_found ->
            match find_loop name t2 with
            | { name; args; fargs; body; ret } ->
              let body' =
                match exp with
                | IfEq _ ->
                  Ans (IfEq (id_t, id_or_imm,
                             Ans (CallDir (Id.L ("min_caml_test_trace"), args, fargs)),
                             body
                            ))
                | IfLE _ ->
                  Ans (IfLE (id_t, id_or_imm,
                             Ans (CallDir (Id.L ("min_caml_test_trace"), args, fargs)),
                             body
                            ))
                | IfGE _ ->
                  Ans (IfGE (id_t, id_or_imm,
                             Ans (CallDir (Id.L ("min_caml_test_trace"), args, fargs)),
                             body
                            ))
                | _ -> assert false
            in
            { name = name; args = args; fargs = fargs; body = body'; ret = ret }
            | exception Not_found -> raise Not_found
        end
      | _ -> raise Not_found
    end

let rec find_nonloop_t name = function
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_start"), args, fargs), body) ->
    Let ((dest, Type.Int),
         CallDir (Id.L (name), args, fargs),
         find_nonloop_t name body)
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_end"), args, fargs), body) ->
    raise Not_found
  | Let ((dest, typ), exp, body) ->
    Let ((dest, typ), exp, find_nonloop_t name body)
  | Ans (exp) ->
    try find_nonloop_exp' name exp with
    | Not_found -> Ans (Nop)

and find_nonloop_exp name = function
  | CallDir (Id.L ("min_caml_loop_end"), _, _) ->
    raise Not_found
  | IfEq (id_t, id_or_imm, t1, t2)
  | IfGE (id_t, id_or_imm, t1, t2)
  | IfLE (id_t, id_or_imm, t1, t2) ->
    begin match find_loop_t name t1 with
      | t ->
        begin match find_loop_t name t2 with
          | t -> raise Not_found
          | exception Not_found -> t2
        end
      | exception Not_found -> t1
    end
  | exp -> Ans (exp)

and find_nonloop_exp' name = function
  | CallDir (Id.L ("min_caml_loop_end"), _, _) ->
    raise Not_found
  | IfEq (id_t, id_or_imm, t1, t2)
  | IfGE (id_t, id_or_imm, t1, t2)
  | IfLE (id_t, id_or_imm, t1, t2) as exp ->
    begin match find_loop_t name t1 with
      | t ->
        begin match exp with
          | IfEq _ ->
            Ans (
              IfEq (
                id_t, id_or_imm,
                Ans (CallDir (Id.L (name), [id_t], [])),
                t2
              ))
          | IfLE _ ->
            Ans (
              IfLE (
                id_t, id_or_imm,
                Ans (CallDir (Id.L (name), [id_t], [])),
                t2
              ))
          | IfGE _ ->
            Ans (
              IfGE (
                id_t, id_or_imm,
                Ans (CallDir (Id.L (name), [id_t], [])),
                t2
              ))
          | _ -> assert false
        end
      | exception Not_found ->
        begin match find_loop_t name t2 with
          | t ->
            begin match exp with
              | IfEq _ ->
                Ans (
                  IfEq (
                    id_t, id_or_imm,
                    Ans (CallDir (Id.L (name), [id_t], [])),
                    t1
                  ))
              | IfLE _ ->
                Ans (
                  IfLE (
                    id_t, id_or_imm,
                    Ans (CallDir (Id.L (name), [id_t], [])),
                    t1
                  ))
              | IfGE _ ->
                Ans (
                  IfGE (
                    id_t, id_or_imm,
                    Ans (CallDir (Id.L (name), [id_t], [])),
                    t1
                  ))
              | _ -> assert false
            end
          | exception Not_found -> raise Not_found
        end
    end
  | exp -> Ans (exp)

let find_nonloop name ({ name = name; args = args; fargs = fargs; body = body; ret = ret }) =
  let body' = find_nonloop_t "test_loop_fun" body in
  { name = name; args = args; fargs = fargs; body = body'; ret = ret }


let rec before_loop_start name = function
  | Let ((dest, typ), CallDir (Id.L ("min_caml_loop_start"), args, fargs), body) ->
    Ans (CallDir (Id.L (name), args, fargs))
  | Let ((dest, typ), exp, body) ->
    Let ((dest, typ), exp, before_loop_start name body)
  | Ans (exp) -> raise Not_found

type loop_condition =
  | If_equal of Id.t * id_or_imm
  | If_less of  Id.t * id_or_imm
  | If_greater of Id.t * id_or_imm

let rec get_loop_end_args = function
  | Ans (CallDir (Id.L ("min_caml_loop_end"), args, fargs)) -> Some (args, fargs)
  | Ans (exp) -> get_loop_end_args' exp
  | Let (_, CallDir (Id.L ("min_caml_loop_end"), args, fargs), body) -> Some (args, fargs)
  | Let (_, exp, body) -> get_loop_end_args body

and get_loop_end_args' = function
  | IfEq (_, _, t1, t2) | IfLE (_, _, t1, t2) | IfGE (_, _, t1, t2) ->
    begin match get_loop_end_args t1 with
      | Some (a, f) -> Some (a, f)
      | None -> begin match get_loop_end_args t2 with
          | Some (a, f) -> Some (a, f)
          | None -> None
        end
    end
  | _ -> None

let rec after_loop_end' = function
  | Ans (exp) ->
    begin match after_loop_end_exp exp with
      | If_equal (id_t, id_or_imm), t -> t
      | If_less (id_t, id_or_imm), t -> t
      | If_greater (id_t, id_or_imm), t -> t
    end
  | Let ((dest, typ), exp, body) ->
    after_loop_end' body

and after_loop_end_exp exp =
  let dummy = "dummy" in
  match exp with
  | IfEq (id_t, id_or_imm, t1, t2)  ->
    (match find_loop_t dummy t1 with
     | _ -> If_equal (id_t, id_or_imm), t2
     | exception Not_found -> If_equal (id_t, id_or_imm), t1)
  | IfLE (id_t, id_or_imm, t1, t2) ->
    (match find_loop_t dummy t1 with
     | _ -> If_less (id_t, id_or_imm), t2
     | exception Not_found -> If_less (id_t, id_or_imm), t1)
  | IfGE (id_t, id_or_imm, t1, t2) ->
    (match find_loop_t dummy t1 with
     | _ -> If_greater (id_t, id_or_imm), t2
     | exception Not_found -> If_greater (id_t, id_or_imm), t1)
  | CallDir (Id.L ("min_caml_loop_end"), args, fargs) ->
    assert false
  | exp -> assert false

let rec after_loop_end name ({ name = _; args = args; fargs = fargs; body = body; ret = ret }) =
  let args', fargs' = match get_loop_end_args body with
    | Some (a, f) -> a, f
    | None -> failwith "getting args/fargs of CallDir (Id.L min_caml_loop_end) is failed."
  in
  let body' = after_loop_end' body in
  { name = Id.L (name); args = args'; fargs = fargs'; body = body'; ret = ret }
