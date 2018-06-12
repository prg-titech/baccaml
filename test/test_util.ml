open Core

open Mincaml
open Baccaml_jit
open Util
open Xutil
open Jit_config
open Jit_util

include Tracing_jit
include Method_jit
include Jit_config
include Mutil

let () =
  Logger.log_level := Logger.Debug

let setup aa bb =
  List.iter
    ~f:(fun (a, i) -> bb.(i) <- value_of a)
    (List.zip (Array.to_list aa) (List.range 0 (Array.length aa - 1)))

module Test_dependencies = struct

end
