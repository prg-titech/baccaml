type env =
  { prog : MinCaml.Asm.prog;
    reg : BacCaml.Jit_config.value array;
    mem : BacCaml.Jit_config.value array;
    red_args : string list;
    ex_name : string;
  }

type arg =
  { file : string;
    ex_name : string;
    code : int array;
    annot : int array;
    reds : (string * int) list;
    greens : (string * int) list;
  }

type var =
  { redtbl : (string, int) Hashtbl.t;
    greentbl : (string, int) Hashtbl.t;
  }

type tenv =
  { fundefs : MinCaml.Asm.fundef list;
    ibody : MinCaml.Asm.t;
  }

val prepare_env : [< `Meta_method | `Meta_tracing] -> arg -> env
val run : ([> `Meta_method | `Meta_tracing] -> arg -> 'a) -> 'a
