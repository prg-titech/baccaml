#include <caml/mlvalues.h>
#include <caml/callback.h>

void dummy_fun(int x) {
  static value * dummy_fun_closure = NULL;
  if (dummy_fun_closure == NULL) dummy_fun_closure = caml_named_value("dummy_fun");
  callback(*dummy_fun, Val_int(n));
  return;
}
