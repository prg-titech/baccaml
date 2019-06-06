#include <stdio.h>
#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

value init_f(int n) {
  return Val_int(n);
}

void call_caml_jit_entry(int **x) {
  static value * jit_entry_closure = NULL;
  value ml_args[6];
  if (jit_entry_closure == NULL) {
    jit_entry_closure = caml_named_value("jit_entry");
  }
  ml_args[0] = caml_alloc_array(init_f, x[0]);
  ml_args[1] = caml_alloc_array(init_f, x[1]);
  ml_args[2] = Val_int(x[2][0]);
  ml_args[3] = Val_int(x[2][1]);
  ml_args[4] = Val_hp(x[0]);
  ml_args[5] = Val_hp(x[1]);
  caml_callbackN(*jit_entry_closure, 6, ml_args);
  return;
}
