#include <stdio.h>
#include <caml/mlvalues.h>

CAMLprim value
caml_two_complement(value v) {
  int i, j;
  i = Int_val(v);
  j = Int_val(v);
  i = j ^ 0xffffffff;
  i = i + 1;
  return Val_int(i);
}
