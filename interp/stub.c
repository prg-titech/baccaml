#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

extern void min_caml_start(char *, char *);

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

/* "stderr" is a macro and cannot be referred to in libmincaml.S, so */
/*    this "min_caml_stderr" is used (in place of "__iob+32") for better */
/*    portability (under SPARC emulators, for example).  Thanks to Steven */
/*    Shaw for reporting the problem and proposing this solution. */
FILE *min_caml_stderr;

int main(int argc, char *argv[]) {
  char *hp, *sp;

  caml_main(argv);

  min_caml_stderr = stderr;
  sp = alloca(1000000); hp = malloc(4000000);
  if (hp == NULL || sp == NULL) {
    fprintf(stderr, "malloc or alloca failed\n");
    return 1;
  }
  /* fprintf(stderr, "sp = %p, hp = %p\n", sp, hp); */
  min_caml_start(sp, hp);

  return 0;
}
