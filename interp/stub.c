#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

extern void min_caml_start(char *, char *);

extern void call_caml_jit_entry(int *, int , int *, int) asm ("call_caml_jit_entry");

extern void call_caml_jit_exec(int, int *, int) asm ("call_caml_jit_exec");

extern int get_current_millis(void) asm ("min_caml_get_current_millis");

value init_f(int n) {
  return Val_int(n);
}

void call_caml_jit_entry(int *st, int sp, int *bc, int pc) {
  static value * jit_entry_closure = NULL;
  value ml_args[6];
  if (jit_entry_closure == NULL) {
    jit_entry_closure = caml_named_value("jit_entry");
  }
  ml_args[0] = caml_alloc_array(init_f, bc);
  ml_args[1] = caml_alloc_array(init_f, st);
  ml_args[2] = Val_int(pc);
  ml_args[3] = Val_int(sp);
  ml_args[4] = Val_hp(bc);
  ml_args[5] = Val_hp(st);
  caml_callbackN(*jit_entry_closure, 6, ml_args);
  return;
}

void call_caml_jit_exec(int pc, int *st_ptr, int sp) {
  static value * jit_exec_closure = NULL;
  if (jit_exec_closure == NULL) {
    jit_exec_closure = caml_named_value("jit_exec");
  }
  caml_callback3(*jit_exec_closure, Val_int(pc), Val_hp(st_ptr), Val_int(sp));
  return;
}

int get_current_millis() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  int time_in_mill = (tv.tv_sec) * 1000 + (tv.tv_usec) / 1000;
  return time_in_mill;
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
