open Base
open Asm

type env = {
    trace_name : string;
    red_args : string list;
    red_names : string list;
    index_pc : int;
    merge_pc : int
  }

let create_env ~trace_name ~red_args ~red_names ~index_pc ~merge_pc =
  { trace_name; red_args; red_names; index_pc; merge_pc }

type value = Red of int | Green of int | LightGreen of int

type reg = value array

type mem = value array

type jit_result = Specialized of value | Not_specialized of exp * value
