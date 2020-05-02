open MinCaml
open Asm
open Opt_lib

let check_sp sp =
  let sp_strs = String.split_on_char '.' sp in
  List.hd sp_strs = "sp" && List.length sp_strs = 2
;;

let check_stack id =
  let stk_strs = String.split_on_char '.' id in
  List.hd stk_strs = "stack" && List.length stk_strs = 2
;;

let rec remove_rw (sp_env : int M.t) (mem_env : string M'.t) = function
  | Let ((var, typ), (Add (x, C n) as e), t) when check_sp x ->
    let sp_env = M.add var n sp_env in
    Let ((var, typ), e, remove_rw sp_env mem_env t)
  | Let ((var, typ), (Sub (x, C n) as e), t) when check_sp x ->
    let sp_env = M.add var (-n) sp_env in
    Let ((var, typ), e, remove_rw sp_env mem_env t)
  | Let ((var, typ), (St (x, y, V z, w) as e), t) when check_stack y && check_sp z ->
    let mem_env = M'.add 0 x mem_env in
    Let ((var, typ), e, t |> remove_rw sp_env mem_env)
  | Let ((var, typ), (St (x, y, V z, w) as e), t) when check_stack y ->
    (try
       let sp = M.find z sp_env in
       let mem_env = M'.add sp x mem_env in
       Let ((var, typ), e, t |> remove_rw sp_env mem_env)
     with
    | Not_found -> Let ((var, typ), e, t |> remove_rw sp_env mem_env))
  | Let ((var, typ), (Ld (x, V y, z) as e), t) when check_stack x && check_sp y ->
    (try
       let addr = M'.find 0 mem_env in
       Let ((var, typ), Mov addr, t |> remove_rw sp_env mem_env)
     with
    | Not_found -> Let ((var, typ), e, t |> remove_rw sp_env mem_env))
  | Let ((var, typ), (Ld (x, V y, z) as e), t) when check_stack x ->
    (try
       let sp = M.find y sp_env in
       let addr = M'.find sp mem_env in
       pp "Removing: Ld (%s, %s, %d) => Mov (%s)\n" x y z addr;
       Let ((var, typ), Mov addr, t |> remove_rw sp_env mem_env)
     with
    | Not_found -> Let ((var, typ), e, t |> remove_rw sp_env mem_env))
  | Let ((var, typ), e, t) -> Let ((var, typ), e, t |> remove_rw sp_env mem_env)
  | Ans (Ld (x, V y, z) as e) when check_stack x && check_sp y ->
    (try
       let addr = M'.find 0 mem_env in
       Ans (Mov addr)
     with
    | Not_found -> Ans e)
  | Ans (Ld (x, V y, z) as e) when check_stack x ->
    (try
       let sp = M.find y sp_env in
       let addr = M'.find sp mem_env in
       Ans (Mov addr)
     with
    | Not_found -> Ans e)
  | Ans (IfEq (x, y, t1, t2) as e)
  | Ans (IfLE (x, y, t1, t2) as e)
  | Ans (IfGE (x, y, t1, t2) as e) ->
    Ans (e <=> (x, y, remove_rw sp_env mem_env t1, remove_rw sp_env mem_env t2))
  | Ans e -> Ans e
;;

let rec find_remove_candidate
    (sp_env : int M.t)
    (mem_env : (string * exp) M'.t)
    (remove_cand : exp M.t)
  = function
  | Let ((var, typ), Add (x, C n), t) when check_sp x ->
    let sp_env = M.add var n sp_env in
    find_remove_candidate sp_env mem_env remove_cand t
  | Let ((var, typ), Sub (x, C n), t) when check_sp x ->
    let sp_env = M.add var (-n) sp_env in
    find_remove_candidate sp_env mem_env remove_cand t
  | Let ((var, typ), (St (x, y, V z, w) as e), t) when check_stack y && check_sp z ->
    (match M'.find_opt 0 mem_env with
    | Some (var', e') ->
      let remove_cand = M.add var' e' remove_cand in
      let mem_env = M'.add 0 (var, e) mem_env in
      t |> find_remove_candidate sp_env mem_env remove_cand
    | None ->
      let mem_env = M'.add 0 (var, e) mem_env in
      t |> find_remove_candidate sp_env mem_env remove_cand)
  | Let ((var, typ), (St (x, y, V z, w) as e), t) when check_stack y ->
    (try
       let sp = M.find z sp_env in
       match M'.find_opt sp mem_env with
       | Some (var', e') ->
         let remove_cand = M.add var' e' remove_cand in
         let mem_env = M'.add sp (var, e) mem_env in
         t |> find_remove_candidate sp_env mem_env remove_cand
       | None ->
         let mem_env = M'.add sp (var, e) mem_env in
         t |> find_remove_candidate sp_env mem_env remove_cand
     with
    | Not_found -> t |> find_remove_candidate sp_env mem_env remove_cand)
  | Let ((var, typ), e, t) -> t |> find_remove_candidate sp_env mem_env remove_cand
  | Ans (IfEq (x, y, t1, t2)) | Ans (IfLE (x, y, t1, t2)) | Ans (IfGE (x, y, t1, t2)) ->
    let f = find_remove_candidate sp_env mem_env remove_cand in
    if Opt_guard.is_guard_path t1 then f t2 else f t1
  | Ans e -> remove_cand
;;

let rec remove_unread_write cand = function
  | Let ((var, typ), e, t) when M.mem var cand -> remove_unread_write cand t
  | Let ((var, typ), e, t) -> Let ((var, typ), e, remove_unread_write cand t)
  | Ans (IfEq (x, y, t1, t2) as e)
  | Ans (IfLE (x, y, t1, t2) as e)
  | Ans (IfGE (x, y, t1, t2) as e) ->
    Ans (e <=> (x, y, remove_unread_write cand t1, remove_unread_write cand t2))
  | Ans e -> Ans e
;;

let const_fold_rw t =
  let t1 = remove_rw M.empty M'.empty t in
  let cand = find_remove_candidate M.empty M'.empty M.empty t1 in
  remove_unread_write cand t1
;;
