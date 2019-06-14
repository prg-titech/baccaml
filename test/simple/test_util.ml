open Base
open Jit

open Jit_config
open Jit_util

open Core

include Tracing_jit
include Method_jit
include Jit_config
include Util

let setup aa bb =
  Core.List.iter
    ~f:(fun (a, i) -> bb.(i) <- value_of a)
    (List.zip_exn
       (Array.to_list aa)
       (List.range 0 (Array.length aa - 1)))
