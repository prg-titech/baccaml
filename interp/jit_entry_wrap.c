#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/major_gc.h>
#include <caml/memory.h>
#include <caml/stacks.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

int call_caml_dummy_fun(int x) {
  static value * f_clsr = NULL;
  if (f_clsr == NULL) {
    f_clsr = caml_named_value("dummy_fun");
  }
  return Int_val(callback(*f_clsr, Val_int(x)));
}

value init_f(int n) {
  return Val_int(n);
}

void call_caml_jit_entry(int **x) {
  static value * jit_entry_closure = NULL;
  if (jit_entry_closure == NULL) {
    jit_entry_closure = caml_named_value("jit_entry");
  }
  caml_callback3(*jit_entry_closure,
                 caml_alloc_array(init_f, x[0]),
                 caml_alloc_array(init_f, x[1]),
                 caml_alloc_array(init_f, x[2]));
  return;
}
