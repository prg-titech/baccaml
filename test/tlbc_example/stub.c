#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>

extern void min_caml_start(char *, char *);
extern void interp_debug(int, int, int);
extern unsigned long get_current_micros(void) asm ("min_caml_get_current_micros");

void interp_debug(int pc, int instr, int sp) {
  fprintf(stderr, "pc: %d, instr: %d, sp: %d\n", pc, instr, sp);
  return;
}

unsigned long get_current_micros() {
  struct timeval current_time;
  gettimeofday(&current_time, NULL);
  return current_time.tv_sec * (int)1e6 + current_time.tv_usec;
}

int divide(int rhs, int lhs) { return rhs / lhs; }

int modulo(int rhs, int lhs) { return rhs % lhs; }

/* "stderr" is a macro and cannot be referred to in libmincaml.S, so */
/*    this "min_caml_stderr" is used (in place of "__iob+32") for better */
/*    portability (under SPARC emulators, for example).  Thanks to Steven */
/*    Shaw for reporting the problem and proposing this solution. */
FILE *min_caml_stderr;
FILE *min_caml_stdout;

int main(int argc, char *argv[]) {
  char *hp, *sp;

  min_caml_stderr = stderr;
  min_caml_stdout = stdout;
  sp = alloca(1000000); hp = malloc(4000000);
  if (hp == NULL || sp == NULL) {
    fprintf(stderr, "malloc or alloca failed\n");
    return 1;
  }
  /* fprintf(stderr, "sp = %p, hp = %p\n", sp, hp); */
  min_caml_start(sp, hp);

  return 0;
}
