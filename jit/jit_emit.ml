open MinCaml
open Asm
open Printf

let emit_mj
    oc
    ({ name = Id.L x; args; fargs = _; body = e; ret = _ } as fundef)
  =
  let cname = Filename.chop_extension x in
  fprintf oc ".code32\n";
  fprintf oc ".balign\t8\n";
  fprintf oc ".text\n";
  fprintf oc ".globl %s\n" cname;
  fprintf oc "%s:\n" cname;
  fprintf oc "\tpushl\t%%eax\n";
  fprintf oc "\tpushl\t%%ebx\n";
  fprintf oc "\tpushl\t%%ecx\n";
  fprintf oc "\tpushl\t%%edx\n";
  fprintf oc "\tpushl\t%%esi\n";
  fprintf oc "\tpushl\t%%edi\n";
  fprintf oc "\tpushl\t%%ebp\n";
  fprintf oc "\tmovl\t32(%%esp),%s\n" regs.(0);
  fprintf oc "\tmovl\t36(%%esp),%s\n" regs.(1);
  fprintf oc "\tcall\t%s\n" x;
  fprintf oc "\tmovl\t%%eax, 32(%%esp)\n";
  fprintf oc "\tpopl\t%%ebp\n";
  fprintf oc "\tpopl\t%%edi\n";
  fprintf oc "\tpopl\t%%esi\n";
  fprintf oc "\tpopl\t%%edx\n";
  fprintf oc "\tpopl\t%%ecx\n";
  fprintf oc "\tpopl\t%%ebx\n";
  fprintf oc "\tpopl\t%%eax\n";
  fprintf oc "\tmovl\t4(%%esp), %%eax\n";
  fprintf oc "\tret\n";
  Emit.h oc fundef;
  fprintf oc "guard_%s:\n" x;
  (* use esp for caliculating an offset *)
  fprintf oc "\tmovl\t%%eax, min_caml_guard_stack\n";
  fprintf oc "\tmovl\t%%ebx, min_caml_guard_sp\n";
  fprintf oc "\tmovl\t%%ecx, min_caml_guard_bytecode\n";
  fprintf oc "\tmovl\t%%edx, min_caml_guard_pc\n";
  fprintf oc "\tmovl\t$1, min_caml_guard_fail_flg\n";
  fprintf oc "\tret\n"
;;

let emit_tj
    oc
    ({ name = Id.L x; args; fargs = _; body = e; ret = _ } as fundef)
  =
  let tname = Filename.chop_extension x in
  fprintf oc ".code32\n";
  fprintf oc ".data\n";
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
  fprintf oc "\tcall\t%s\n" x;
  fprintf oc ".globl debug_%s\n" x;
  fprintf oc "debug_%s:\n" x;
  fprintf oc "\tpopl\t%%ebp\n";
  fprintf oc "\tpopl\t%%edi\n";
  fprintf oc "\tpopl\t%%esi\n";
  fprintf oc "\tpopl\t%%edx\n";
  fprintf oc "\tpopl\t%%ecx\n";
  fprintf oc "\tpopl\t%%ebx\n";
  fprintf oc "\tpopl\t%%eax\n";
  fprintf oc "\tret\n";
  Emit.h oc fundef;
  fprintf oc "guard_%s:\n" x;
  (* use esp for caliculating an offset *)
  fprintf oc "\tmovl\t%%eax, min_caml_guard_stack\n";
  fprintf oc "\tmovl\t%%ebx, min_caml_guard_sp\n";
  fprintf oc "\tmovl\t%%ecx, min_caml_guard_bytecode\n";
  fprintf oc "\tmovl\t%%edx, min_caml_guard_pc\n";
  fprintf oc "\tmovl\t$1, min_caml_guard_fail_flg\n";
  (* fprintf oc "\tcall\tmin_caml_guard_occur_at\n"; *)
  fprintf oc "\tret\n"
;;

let h typ oc fundef =
  match typ with
  | `Meta_tracing -> emit_tj oc fundef
  | `Meta_method -> emit_mj oc fundef
;;
