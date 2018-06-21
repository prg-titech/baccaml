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
  { mutable trace_name : string;
    mutable reds : string list;
    mutable greens: string list;
    mutable loop_header : int;
    mutable loop_pc_place : int
  }
  
type method_jit_args =
  { mutable method_name : string;
    mutable reds : string list;
    mutable method_start : int;
    mutable method_end : int;
    mutable pc_place : int;
    mutable loop_headers : int list;
    mutable backedge_pcs : int list
  }

type jit_args =
  | Tracing_jit_args of tracing_jit_args
  | Method_jit_args of method_jit_args

type trace_result =
  | Tracing_success of fundef
  | Method_success of fundef

let zero = "zero.0"
