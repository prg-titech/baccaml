#ifndef RUNTIMR_CAML_H
#define RUNTIME_CAML_H

#include <caml/mlvalues.h>

value call_caml_jit_tracing(int *, int, int *, int);

value call_caml_jit_method(int *, int, int *, int);

value call_caml_jit_setup_tj(int *, int, int *, int);

value call_caml_jit_setup_mj(int *, int, int *, int);

#endif /* RUNTIME_CAML_H */
