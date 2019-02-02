#include <caml/mlvalues.h>
#include <caml/callback.h>

int call_caml_dummy_fun(int x) {
  static value * f_clsr = NULL;
  if (f_clsr == NULL) {
    f_clsr = caml_named_value("dummy_fun");
  }
  return Int_val(callback(*f_clsr, Val_int(x)));
}
