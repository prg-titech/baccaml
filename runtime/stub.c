#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

extern void min_caml_start(char *, char *);

extern int get_current_millis(void) asm ("min_caml_get_current_millis");

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