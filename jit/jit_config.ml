open Mincaml
open Asm
open Core

type value =
  | Red of int
  | Green of int
  | LightGreen of int

type reg = value array

type mem = value array

type jit_result =
  | Specialized of value
  | Not_specialized of exp * value

type tracing_jit_args =
  { trace_name : string;
    reds : string list;
    greens: string list;
    loop_header : int;
    loop_pc_place : int
  }

type method_jit_args =
  { method_name : string;
    reds : string list;
    method_start : int;
    method_end : int;
    pc_place : int;
    backedge_pcs : int list
  }

type jit_args =
  | Tracing_jit_args of tracing_jit_args
  | Method_jit_args of method_jit_args

type trace_result =
  | Tracing_success of fundef
  | Method_success of fundef



let zero = "zero.0"
