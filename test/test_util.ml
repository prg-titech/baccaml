open Mincaml
open Baccaml_jit
open Util
open Jit_config

include Tracing_jit
include Method_jit
include Jit_config
include Mincaml_util

let () =
  Logger.log_level := Logger.Debug

let setup aa bb =
  List.iter
    (fun (a, i) -> bb.(i) <- value_of a)
    (ListUtil.zip (Array.to_list aa) (ListUtil.range 0 (Array.length aa - 1)))

module Test_dependencies = struct

end
