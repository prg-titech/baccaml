#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <unistd.h>

value init_f(long n) { return Val_long(n); }

value init_g(long n) {
  if (n == -1024) {
    return Val_long(0);
  } else {
    return Val_long(n);
  }
}

void call_caml_jit_entry(long *st, long sp, long *bc, long pc) {
  static value *jit_tracing_entry_closure = NULL;
  value ml_args[6];
  if (jit_tracing_entry_closure == NULL) {
    jit_tracing_entry_closure = caml_named_value("jit_tracing_entry");
  }
  ml_args[0] = caml_alloc_array((value (*)(char const *))init_f, (char const **)bc);
  ml_args[1] = caml_alloc_array((value (*)(char const *))init_f, (char const **)st);
  ml_args[2] = Val_long(pc);
  ml_args[3] = Val_long(sp);
  ml_args[4] = Val_hp(bc);
  ml_args[5] = Val_hp(st);
  caml_callbackN(*jit_tracing_entry_closure, 6, ml_args);
  return;
}

void call_caml_jit_exec(long *st_ptr, long sp, long* code_ptr, long pc) {
  static value *jit_exec_closure = NULL;
  value ml_args[4];
  if (jit_exec_closure == NULL) {
    jit_exec_closure = caml_named_value("jit_tracing_exec");
  }
  ml_args[0] = Val_long(pc);
  ml_args[1] = Val_hp(st_ptr);
  ml_args[2] = Val_long(sp);
  ml_args[3] = caml_alloc_array((value (*)(char const *))init_f, (char const **)st_ptr);
  caml_callbackN(*jit_exec_closure, 4, ml_args);
  return;
}

void call_caml_guard_occur_at(long *st, long sp, long *bc, long pc) {
  static value *jit_guard_occur_at_clsr = NULL;
  value ml_args[6];
  if (jit_guard_occur_at_clsr == NULL) {
    jit_guard_occur_at_clsr = caml_named_value("jit_guard_occur_at");
  }
  ml_args[0] = caml_alloc_array((value (*)(char const *))init_f, (char const **)bc);
  ml_args[1] = caml_alloc_array((value (*)(char const *))init_f, (char const **)st);
  ml_args[2] = Val_long(pc);
  ml_args[3] = Val_long(sp);
  ml_args[4] = Val_hp(bc);
  ml_args[5] = Val_hp(st);
  caml_callbackN(*jit_guard_occur_at_clsr, 6, ml_args);
  return;
}

long call_caml_mj_call(long *st, long sp, long *bc, long pc) {
  static value *jit_method_call_closure = NULL;
  value ml_args[6];
  if (jit_method_call_closure == NULL) {
    jit_method_call_closure = caml_named_value("jit_method_call");
  }
  ml_args[0] = caml_alloc_array((value (*)(char const *))init_f, (char const **)bc);
  ml_args[1] = caml_alloc_array((value (*)(char const *))init_f, (char const **)st);
  ml_args[2] = Val_long(pc);
  ml_args[3] = Val_long(sp);
  ml_args[4] = Val_hp(bc);
  ml_args[5] = Val_hp(st);
  return Long_val(caml_callbackN(*jit_method_call_closure, 6, ml_args));
}
