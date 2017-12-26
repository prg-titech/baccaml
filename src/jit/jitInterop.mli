val find_in_body : Asm.t -> Core.String.t Core.List.t -> Id.t Core.List.t
val find_in_fundefs :
  Asm.fundef Core.List.t -> Core.String.t Core.List.t -> Id.t Core.List.t
val find_registers :
  Asm.prog -> Core.String.t Core.List.t -> Id.t Core.List.t
val convert :
  Asm.prog ->
  int Core.Array.t ->
  Core.String.t Core.List.t ->
  Core.String.t Core.List.t ->
  JitConfig.value Core.Array.t
