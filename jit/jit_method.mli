open MinCaml
open Asm
open Jit_env

val run : prog -> reg -> mem -> env -> [`Result of fundef * string list option]
