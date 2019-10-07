open Base
open Asm

type env = {
    trace_name : string;
    red_names : string list;
    index_pc : int;
    merge_pc : int;
    bytecode : int array;
  }

let create_env ~trace_name ~red_names ~index_pc ~merge_pc ~bytecode =
  { trace_name; red_names; index_pc; merge_pc; bytecode }

type value = Red of int | Green of int | LightGreen of int

type reg = value array

type mem = value array

type jit_result = Specialized of value | Not_specialized of exp * value
