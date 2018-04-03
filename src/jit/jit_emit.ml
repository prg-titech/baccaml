open Asm
open Core
open Emit

let replace input output =
  Str.global_replace (Str.regexp_string input) output

let emit_asm fundef fname =
  Format.printf "compiling %s...\n" fname;
  fundef
  |> RegAlloc.h
  |> Emit.h (Out_channel.create (fname ^ ".s"))

let emit_trace fundef fname iname =
  let { name; body } = fundef |> RegAlloc.h in
  let ic = In_channel.create (fname ^ ".s") in
  let oc = Out_channel.create (fname ^ ".s") in
  let Id.L (x) = name in
  Printf.fprintf oc ".globl %s\n" x;
  Printf.fprintf oc "%s:\n" x;
  Printf.fprintf oc "\tpushl\t%%ebx\n";
  Printf.fprintf oc ".globl %s.1\n" x;
  Printf.fprintf oc "%s1:" x;
  g oc (Tail, body);
  Printf.fprintf oc ".globl %s2\n" x;
  Printf.fprintf oc "%s2:\n" x;
  Printf.fprintf oc "\tpopl\t%%eax\n";
  Printf.fprintf oc "jmp\t%s" iname

