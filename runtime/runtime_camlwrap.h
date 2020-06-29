#ifndef RUNTIMR_CAML_H
#define RUNTIME_CAML_H

#include <caml/mlvalues.h>

value call_caml_jit_tracing(int *, int, int *, int) asm ("call_caml_jit_tracing");

#endif /* RUNTIME_CAML_H */
