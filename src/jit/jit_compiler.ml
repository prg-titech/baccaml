open Asm
open Core

let compile prog fname =
  Format.printf "compiling %s...\n" fname;
  prog
  |> Simm.f
  |> RegAlloc.f
  |> Emit.f (Out_channel.create fname);
  let exit_code = Sys.command (Format.sprintf "gcc -c -m32 %s" fname) in
  if exit_code <> 0 then
    failwith (Format.sprintf "compile %s is failed\n" fname)
  else
    Format.printf "compile %s is succeeded\n" fname;
    ()
