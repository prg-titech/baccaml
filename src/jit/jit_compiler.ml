open Asm
open Core

let compile (fundef : fundef) (fname : string) =
  Format.printf "compiling %s...\n" fname;
  fundef
  |> RegAlloc.h
  |> Emit.h (Out_channel.create fname);
  let exit_code = Sys.command (Format.sprintf "gcc -c -m32 %s" fname) in
  if exit_code <> 0 then
    failwith (Format.sprintf "compile %s is failed\n" fname)
  else
    Format.printf "compiling %s is succeeded\n" fname

let gcc (fname : string) (mname : string) (ename: string) =
  let lib_mincaml = "lib/libmincaml.S" in
  let stub = "lib/stub.c" in
  let cmd = Format.sprintf "gcc -g -O2 -Wall %s %s %s %s -lm -o %s" lib_mincaml stub fname mname ename in
  match Sys.command cmd with
  | 0 -> Format.printf "%s is succeeded.\n" cmd
  | _ -> Format.printf "%s is failed.\n" cmd
