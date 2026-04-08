#include <stdio.h>
#include <stdint.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>

value get_float_bits(value v) {
  union {
    double d;
    int64_t i;
  } u;
  u.d = Double_val(v);
  return caml_copy_int64(u.i);
}
