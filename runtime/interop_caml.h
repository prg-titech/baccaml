#ifndef INTEROP_CAML_H
#define INTEROP_CAML_H

void call_caml_jit_entry(int *, int, int *, int) asm("call_caml_jit_entry");

void call_caml_jit_exec(int, int *, int) asm("call_caml_jit_exec");

void call_caml_guard_occur_at(int *, int, int *, int) asm("call_caml_guard_occur_at");

int call_caml_jit_mj_call(int *, int, int *, int) asm("call_caml_mj_call");

#endif /* INTEROP_CAML_H */
