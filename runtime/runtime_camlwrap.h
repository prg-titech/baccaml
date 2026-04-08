#ifndef RUNTIME_CAML_H
#define RUNTIME_CAML_H

#include <caml/mlvalues.h>

value call_caml_jit_tracing(long *, long, long *, long);

value call_caml_jit_method(long *, long, long *, long);

value call_caml_jit_setup_tj(long *, long, long *, long);

value call_caml_jit_setup_mj(long *, long, long *, long);

#endif /* RUNTIME_CAML_H */
