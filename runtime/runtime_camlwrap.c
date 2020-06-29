#include <caml/alloc.h>
#include <caml/compatibility.h>
#include <caml/memory.h>

#include <caml/callback.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

value f(int n) { return Val_int(n); }

value call_caml_jit_tracing(int *stack, int sp, int *bytecode, int pc) {
  static value *jit_tracing_closure = NULL;
  value v, x, y;

  value ml_args[6];
  if (jit_tracing_closure == NULL) {
    jit_tracing_closure = caml_named_value("caml_jit_tracing");
  }
  ml_args[0] = caml_alloc_array(f, bytecode);
  ml_args[1] = caml_alloc_array(f, stack);
  ml_args[2] = Val_int(pc);
  ml_args[3] = Val_int(sp);
  ml_args[4] = Val_hp(bytecode);
  ml_args[5] = Val_hp(stack);

  return caml_callbackN(*jit_tracing_closure, 6, ml_args);
  // ==> string, string array, int
}
