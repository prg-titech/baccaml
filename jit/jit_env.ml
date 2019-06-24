open Base
open Asm

type env = {
    trace_name : string;
    red_args : string list;
    index_pc : int;
    merge_pc : int
  }


type value = Red of int | Green of int | LightGreen of int

type reg = value array

type mem = value array

type jit_result = Specialized of value | Not_specialized of exp * value
