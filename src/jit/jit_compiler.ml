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
    Format.printf "compiling %s is succeeded\n" fname;
    ()
