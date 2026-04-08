open MinCaml
open Asm
open Printf

let emit_mj
    oc
    ({ name = Id.L x; args; fargs = _; body = e; ret = _ } as fundef)
  =
  let cname = Filename.chop_extension x in
  fprintf oc ".data\n";
  fprintf oc ".balign\t16\n";
  fprintf oc ".text\n";
  fprintf oc ".globl %s\n" cname;
  fprintf oc "%s:\n" cname;
  (* Save callee-saved registers *)
  fprintf oc "\tpushq\t%%rbx\n";
  fprintf oc "\tpushq\t%%rbp\n";
  fprintf oc "\tpushq\t%%r12\n";
  fprintf oc "\tpushq\t%%r13\n";
  fprintf oc "\tpushq\t%%r14\n";
  fprintf oc "\tpushq\t%%r15\n";
  (* System V ABI: first arg in %rdi, second in %rsi -> MinCaml regs *)
  fprintf oc "\tmovq\t%%rdi,%s\n" regs.(0);
  fprintf oc "\tmovq\t%%rsi,%s\n" regs.(1);
  fprintf oc "\tcall\t%s\n" x;
  fprintf oc ".globl debug_%s\n" x;
  fprintf oc "debug_%s:\n" x;
  (* Return value already in %rax *)
  fprintf oc "\tmovq\t%%rax, %%rdi\n";
  (* Restore callee-saved registers *)
  fprintf oc "\tpopq\t%%r15\n";
  fprintf oc "\tpopq\t%%r14\n";
  fprintf oc "\tpopq\t%%r13\n";
  fprintf oc "\tpopq\t%%r12\n";
  fprintf oc "\tpopq\t%%rbp\n";
  fprintf oc "\tpopq\t%%rbx\n";
  fprintf oc "\tmovq\t%%rdi, %%rax\n";
  fprintf oc "\tret\n";
  Emit.h oc fundef;
  fprintf oc "guard_%s:\n" x;
  fprintf oc "\tmovq\t%%rax, min_caml_guard_stack(%%rip)\n";
  fprintf oc "\tmovq\t%%rbx, min_caml_guard_sp(%%rip)\n";
  fprintf oc "\tmovq\t%%rcx, min_caml_guard_bytecode(%%rip)\n";
  fprintf oc "\tmovq\t%%rdx, min_caml_guard_pc(%%rip)\n";
  fprintf oc "\tmovq\t$1, min_caml_guard_fail_flg(%%rip)\n";
  fprintf oc "\tret\n"
;;

let emit_tj
    oc
    ({ name = Id.L x; args; fargs = _; body = e; ret = _ } as fundef)
  =
  let tname = Filename.chop_extension x in
  fprintf oc ".data\n";
  fprintf oc ".balign\t16\n";
  fprintf oc ".text\n";
  fprintf oc ".globl %s\n" tname;
  fprintf oc "%s:\n" tname;
  (* Save callee-saved registers *)
  fprintf oc "\tpushq\t%%rbx\n";
  fprintf oc "\tpushq\t%%rbp\n";
  fprintf oc "\tpushq\t%%r12\n";
  fprintf oc "\tpushq\t%%r13\n";
  fprintf oc "\tpushq\t%%r14\n";
  fprintf oc "\tpushq\t%%r15\n";
  (* System V ABI: first arg in %rdi, second in %rsi -> MinCaml regs *)
  fprintf oc "\tmovq\t%%rdi,%s\n" regs.(0);
  fprintf oc "\tmovq\t%%rsi,%s\n" regs.(1);
  fprintf oc "\tcall\t%s\n" x;
  fprintf oc ".globl debug_%s\n" x;
  fprintf oc "debug_%s:\n" x;
  (* Restore callee-saved registers *)
  fprintf oc "\tpopq\t%%r15\n";
  fprintf oc "\tpopq\t%%r14\n";
  fprintf oc "\tpopq\t%%r13\n";
  fprintf oc "\tpopq\t%%r12\n";
  fprintf oc "\tpopq\t%%rbp\n";
  fprintf oc "\tpopq\t%%rbx\n";
  fprintf oc "\tret\n";
  Emit.h oc fundef;
  fprintf oc "guard_%s:\n" x;
  fprintf oc "\tmovq\t%%rax, min_caml_guard_stack(%%rip)\n";
  fprintf oc "\tmovq\t%%rbx, min_caml_guard_sp(%%rip)\n";
  fprintf oc "\tmovq\t%%rcx, min_caml_guard_bytecode(%%rip)\n";
  fprintf oc "\tmovq\t%%rdx, min_caml_guard_pc(%%rip)\n";
  fprintf oc "\tmovq\t$1, min_caml_guard_fail_flg(%%rip)\n";
  fprintf oc "\tret\n"
;;

let h typ oc fundef =
  match typ with
  | `Meta_tracing -> emit_tj oc fundef
  | `Meta_method -> emit_mj oc fundef
;;
