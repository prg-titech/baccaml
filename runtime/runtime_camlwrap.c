#include <caml/alloc.h>
#include <caml/memory.h>

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

/* For JIT compilation */

value f(long n) { return Val_long(n); }

value call_caml_jit_tracing(long *stack, long sp, long *bytecode, long pc) {
  static value *jit_tracing_closure = NULL;
  value v, x, y;

  value ml_args[6];
  if (jit_tracing_closure == NULL) {
    jit_tracing_closure = caml_named_value("caml_jit_tracing");
  }
  ml_args[0] = caml_alloc_array((value (*)(char const *))f, (char const **)bytecode);
  ml_args[1] = caml_alloc_array((value (*)(char const *))f, (char const **)stack);
  ml_args[2] = Val_long(pc);
  ml_args[3] = Val_long(sp);
  ml_args[4] = Val_hp(bytecode);
  ml_args[5] = Val_hp(stack);

  return caml_callbackN(*jit_tracing_closure, 6, ml_args);
  // ==> string, string array, int
}

value call_caml_jit_method(long *stack, long sp, long *bytecode, long pc) {
  static value *jit_tracing_closure = NULL;
  value v, x, y;

  value ml_args[6];
  if (jit_tracing_closure == NULL) {
    jit_tracing_closure = caml_named_value("caml_jit_method");
  }
  ml_args[0] = caml_alloc_array((value (*)(char const *))f, (char const **)bytecode);
  ml_args[1] = caml_alloc_array((value (*)(char const *))f, (char const **)stack);
  ml_args[2] = Val_long(pc);
  ml_args[3] = Val_long(sp);
  ml_args[4] = Val_hp(bytecode);
  ml_args[5] = Val_hp(stack);

  return caml_callbackN(*jit_tracing_closure, 6, ml_args);
  // ==> string, string array, int
}

value call_caml_jit_setup_tj(long *st, long sp, long *bc, long pc) {
  static value *closure = NULL;
  value ml_args[6];
  if (closure == NULL) {
    closure = caml_named_value("caml_jit_setup_tj");
  }
  ml_args[0] = caml_alloc_array((value (*)(char const *))f, (char const **)bc);
  ml_args[1] = caml_alloc_array((value (*)(char const *))f, (char const **)st);
  ml_args[2] = Val_long(pc);
  ml_args[3] = Val_long(sp);
  ml_args[4] = Val_hp(bc);
  ml_args[5] = Val_hp(st);
  return caml_callbackN(*closure, 6, ml_args);
}

value call_caml_jit_setup_mj(long *st, long sp, long *bc, long pc) {
  static value *closure = NULL;
  value ml_args[6];
  if (closure == NULL) {
    closure = caml_named_value("caml_jit_setup_mj");
  }
  ml_args[0] = caml_alloc_array((value (*)(char const *))f, (char const **)bc);
  ml_args[1] = caml_alloc_array((value (*)(char const *))f, (char const **)st);
  ml_args[2] = Val_long(pc);
  ml_args[3] = Val_long(sp);
  ml_args[4] = Val_hp(bc);
  ml_args[5] = Val_hp(st);
  return caml_callbackN(*closure, 6, ml_args);
}
