open Ocamlbuild_plugin

let _ = dispatch (function
    | After_rules ->
      pdep ["link"] "linkdep" (fun param -> [param])
    | _ -> ())
