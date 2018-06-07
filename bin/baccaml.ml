open Core

open Mincaml
open Mutil
open Baccaml_jit
open Jit_config
open Jit_util
open Asm

exception No_function_defs of string
exception Jit_failed of string

module TJ = Tracing_jit
module MJ = Method_jit

let is_tracing = ref true
let is_method = ref false

let rec get_all_vars (p : prog) : Id.t list =
  match p with Prog (tables, fundefs, main) ->
    List.fold_left
      fundefs
      ~f:begin fun acc fundef ->
        let { args; body } = fundef in
        acc @ args @ (get_free_vars_t body)
      end
      ~init:[]

and get_free_vars_t (t : t) : Id.t list =
  match t with
  | Ans (exp) -> get_free_vars_exp exp
  | Let ((dest, _), e, t) -> (dest :: (get_free_vars_exp e)) @ (get_free_vars_t t)

and get_free_vars_exp (e : exp) : Id.t list =
  match e with
  | Mov (id) -> [id]
  | Add (id_t, V (id))
  | Sub (id_t, V (id)) ->  [id_t; id]
  | Add (id_t, C _) | Sub (id_t, C _) -> [id_t]
  | Ld (dest, V (offset), _) -> [dest; offset]
  | Ld (dest, C (_), _) -> [dest]
  | St (src, dest, V (offset), _) -> [src; dest; offset]
  | St (src, dest, C (_), _) -> [src; dest]
  | IfEq (id_t1, V (id_t2), _, _)
  | IfLE (id_t1, V (id_t2), _, _)
  | IfGE (id_t1, V (id_t2), _, _) -> [id_t1; id_t2]
  | IfEq (id_t1, C (_), _, _)
  | IfLE (id_t1, C (_), _, _)
  | IfGE (id_t1, C (_), _, _) -> [id_t1]
  | CallDir (id_l, args, fargs) -> args
  | _ -> []

let colorize_regs
    ~rgs:(regs : value array)
    ~vs:(vars : Id.t list)
    ~g:(greens : (int * string) array)
    ~r:(reds : (int * string) array) : value array =
  List.iter
    vars
    begin fun var ->
      Array.iter reds
        begin fun (i, s) ->
          if s = var then regs.(int_of_id_t var) <- Red (i)
        end;
      Array.iter greens
      begin fun (i, s) ->
        if s = var then regs.(int_of_id_t var) <- Green (i)
      end;
    end;
    regs

let colorize_mem
    ~mems:(mems : value array)
    ~prgs:(program : int array)
    ~g:(greens : (int * string) array)
    ~r:(reds : (int * string) array)
    ~bc:(bytecode : int) : value array =
  for i = 0 to (Array.length program - 1) do
    let n = i * 4 in
    mems.(bytecode + n) <- Green (program.(i))
  done;
  mems

let entry
~fname:(fname : string)
~iname:(iname: string)
~reds:(reds : string list)
~greends:(greens: string list) : unit =
  let regs = Array.create 10000 (Red (0)) in
  let mems = Array.create 10000 (Red (0)) in
  ()

let _ =
  let files = ref [] in
  Arg.parse
    [("--method-jit", Arg.Unit(fun _ -> is_method := true), "enable method jit");
     ("--tracing-jit", Arg.Unit(fun _ -> is_tracing := true), "enable tracing jit");]
    (fun s -> files := !files @ [s])
    ("BacCaml: an experimental meta-hybrid JIT compiler");
  List.iter
    ~f:(fun f -> ())
    !files

