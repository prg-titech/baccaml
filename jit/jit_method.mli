open Base
open Asm
open Jit_env

val run : prog -> reg -> mem -> env -> fundef
val run_multi : prog -> reg -> mem -> env -> fundef list
