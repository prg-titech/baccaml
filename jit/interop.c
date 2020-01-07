#include "caml/compatibility.h"
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>

extern void call_caml_jit_entry(int *, int, int *, int) asm("call_caml_jit_entry");
extern void call_caml_jit_exec(int, int *, int) asm("call_caml_jit_exec");
extern int call_caml_jit_mj_call(int *, int, int *, int) asm("call_caml_mj_call");

value init_f(int n) { return Val_int(n); }

value init_g(int n) {
  if (n == -1024) {
    return Val_int(0);
  } else {
    return Val_int(n);
  }
}

void call_caml_jit_entry(int *st, int sp, int *bc, int pc) {
  static value *jit_tracing_entry_closure = NULL;
  value ml_args[6];
  if (jit_tracing_entry_closure == NULL) {
    jit_tracing_entry_closure = caml_named_value("jit_tracing_entry");
  }
  ml_args[0] = caml_alloc_array(init_f, bc);
  ml_args[1] = caml_alloc_array(init_f, st);
  ml_args[2] = Val_int(pc);
  ml_args[3] = Val_int(sp);
  ml_args[4] = Val_hp(bc);
  ml_args[5] = Val_hp(st);
  caml_callbackN(*jit_tracing_entry_closure, 6, ml_args);
  return;
}

void call_caml_jit_exec(int pc, int *st_ptr, int sp) {
  static value *jit_exec_closure = NULL;
  value ml_args[4];
  if (jit_exec_closure == NULL) {
    jit_exec_closure = caml_named_value("jit_tracing_exec");
  }
  ml_args[0] = Val_int(pc);
  ml_args[1] = Val_hp(st_ptr);
  ml_args[2] = Val_int(sp);
  ml_args[3] = caml_alloc_array(init_f, st_ptr);
  caml_callbackN(*jit_exec_closure, 4, ml_args);
  return;
}

int call_caml_mj_call(int *st, int sp, int *bc, int pc) {
  static value *jit_method_call_closure = NULL;
  value ml_args[6];
  if (jit_method_call_closure == NULL) {
    jit_method_call_closure = caml_named_value("jit_method_call");
  }
  ml_args[0] = caml_alloc_array(init_f, bc);
  ml_args[1] = caml_alloc_array(init_f, st);
  ml_args[2] = Val_int(pc);
  ml_args[3] = Val_int(sp);
  ml_args[4] = Val_hp(bc);
  ml_args[5] = Val_hp(st);
  return Int_val(caml_callbackN(*jit_method_call_closure, 6, ml_args));
}
