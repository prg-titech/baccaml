open Std
open MinCaml
open Jit
open Jit_env
module I = Config.Internal

type t =
  { bytecode : int array
  ; stack : int array
  ; pc : int
  ; sp : int
  ; bc_ptr : int
  ; st_ptr : int
  }

let interp_ir : Asm.prog option ref = ref None
let interp_fundef : Asm.fundef option ref = ref None
let reg = Array.make !I.size (Red 0)
let mem = Array.make !I.size (Green 0)

module Setup = struct
  open Asm

  let get_ir_addr args name =
    List.find (fun a -> String.get_name a = name) args
    |> String.get_extension
    |> int_of_string
  ;;

  let make_reg { args; body = t } =
    let open Jit_env in
    Asm.fv t @ args
    |> List.iteri (fun i a ->
           if List.mem (String.get_name a) !Config.greens
           then reg.(i) <- Green 0
           else reg.(i) <- Red 0);
    ()
  ;;

  let make_mem ~bc_addr ~st_addr bytecode stack =
    let open Jit_env in
    bytecode |> Array.iteri (fun i a -> mem.(bc_addr + (4 * i)) <- Green a);
    stack |> Array.iteri (fun i a -> mem.(st_addr + (4 * i)) <- Red a);
    ()
  ;;

  let env { bytecode; stack; pc; sp; bc_ptr; st_ptr } typ interp =
    let open Asm in
    let open Util in
    let { args; body } = interp in
    let _ = make_reg interp in
    let _ =
      I.(make_mem ~bc_addr:bc_tmp_addr ~st_addr:st_tmp_addr bytecode stack)
    and pc_method_entry = pc
    and pc_ir_addr = get_ir_addr args "pc"
    and sp_ir_addr = get_ir_addr args "sp"
    and bc_ir_addr = get_ir_addr args "bytecode"
    and st_ir_addr = get_ir_addr args "stack" in
    reg.(pc_ir_addr) <- Green pc_method_entry;
    reg.(sp_ir_addr) <- Red sp;
    reg.(bc_ir_addr) <- Green I.bc_tmp_addr;
    reg.(st_ir_addr) <- Red I.st_tmp_addr;
    ()
  ;;
end
