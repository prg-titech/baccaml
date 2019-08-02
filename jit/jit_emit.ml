open Base
open Asm
open Printf

let emit oc typ ({name= Id.L x; args; fargs= _; body= e; ret= _} as fundef) =
  let cname = Filename.chop_extension x in
  Printf.fprintf oc ".code32\n";
  Printf.fprintf oc ".data\n";
  (match typ with
   | `Meta_method ->
     Printf.fprintf oc "mj_result:\n";
     Printf.fprintf oc "\t.long\t0x0\n";
   | `Meta_tracing -> ());
  Printf.fprintf oc ".balign\t8\n";
  Printf.fprintf oc ".text\n";
  Printf.fprintf oc ".globl %s\n" cname;
  Printf.fprintf oc "%s:\n" cname;
  Printf.fprintf oc "\tpushl\t%%eax\n";
  Printf.fprintf oc "\tpushl\t%%ebx\n";
  Printf.fprintf oc "\tpushl\t%%ecx\n";
  Printf.fprintf oc "\tpushl\t%%edx\n";
  Printf.fprintf oc "\tpushl\t%%esi\n";
  Printf.fprintf oc "\tpushl\t%%edi\n";
  Printf.fprintf oc "\tpushl\t%%ebp\n";
  Printf.fprintf oc "\tmovl\t32(%%esp),%s\n" regs.(0);
  Printf.fprintf oc "\tmovl\t36(%%esp),%s\n" regs.(1);
  Printf.fprintf oc "\tcall\t%s\n" x;
  (match typ with
   | `Meta_method ->
     Printf.fprintf oc "\tmovl\t%%eax, mj_result\n"
   | `Meta_tracing ->
     ());
  Printf.fprintf oc "\tpopl\t%%ebp\n";
  Printf.fprintf oc "\tpopl\t%%edi\n";
  Printf.fprintf oc "\tpopl\t%%esi\n";
  Printf.fprintf oc "\tpopl\t%%edx\n";
  Printf.fprintf oc "\tpopl\t%%ecx\n";
  Printf.fprintf oc "\tpopl\t%%ebx\n";
  Printf.fprintf oc "\tpopl\t%%eax\n";
  (match typ with
   | `Meta_method ->
     Printf.fprintf oc "\tmovl\tmj_result, %%eax\n";
   | `Meta_tracing ->
     ());
  Printf.fprintf oc "\tret\n";
  Emit.h oc fundef

let emit_tj oc ({ name = Id.L x; args; fargs= _; body= e; ret= _} as fundef) =
  let tname = Filename.chop_extension x in
  fprintf oc ".code32\n";
  fprintf oc ".data\n";
  fprintf oc "tj_arg1:\n";
  fprintf oc "\t.long\t0x0\n";
  fprintf oc "tj_arg2:\n";
  fprintf oc "\t.long\t0x0\n";
  fprintf oc ".balign\t8\n";
  fprintf oc "tj_arg3:\n";
  fprintf oc "\t.long\t0x0\n";
  fprintf oc "tj_arg4:\n";
  fprintf oc "\t.long\t0x0\n";
  fprintf oc ".balign\t8\n";
  fprintf oc ".text\n";
  fprintf oc ".globl %s\n" tname;
  fprintf oc "%s:\n" tname;
  fprintf oc "\tpushl\t%%eax\n";
  fprintf oc "\tpushl\t%%ebx\n";
  fprintf oc "\tpushl\t%%ecx\n";
  fprintf oc "\tpushl\t%%edx\n";
  fprintf oc "\tpushl\t%%esi\n";
  fprintf oc "\tpushl\t%%edi\n";
  fprintf oc "\tpushl\t%%ebp\n";
  fprintf oc "\tmovl\t32(%%esp),%s\n" regs.(0);
  fprintf oc "\tmovl\t36(%%esp),%s\n" regs.(1);
  fprintf oc "\tjmp\t%s\n" x;
  Emit.h oc fundef

let restore oc p tname =
  let { name = Id.L x; } = Fundef.find_fuzzy p "interp" in
  fprintf oc "guard_%s:\n" tname;
  fprintf oc "\tmovl\t%%eax,tj_arg1\n";
  fprintf oc "\tmovl\t%%ebx,tj_arg2\n";
  fprintf oc "\tmovl\t%%ecx,tj_arg3\n";
  fprintf oc "\tmovl\t%%edx,tj_arg4\n";
  fprintf oc "\tpopl\t%%ebp\n";
  fprintf oc "\tpopl\t%%edi\n";
  fprintf oc "\tpopl\t%%esi\n";
  fprintf oc "\tpopl\t%%edx\n";
  fprintf oc "\tpopl\t%%ecx\n";
  fprintf oc "\tpopl\t%%ebx\n";
  fprintf oc "\tpopl\t%%eax\n";
  fprintf oc "\tmovl\ttj_arg4,%%edx\n";
  fprintf oc "\tmovl\ttj_arg3,%%ecx\n";
  fprintf oc "\tmovl\ttj_arg2,%%ebx\n";
  fprintf oc "\tmovl\ttj_arg1,%%eax\n";
  fprintf oc "\tjmp\t%s\n" x
