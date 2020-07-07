#include <argp.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

#include "runtime.h"
#include "runtime_camlwrap.h"

extern void min_caml_start(char *, char *);

extern unsigned long get_current_micros(void) asm ("min_caml_get_current_micros");

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

static char doc[] = "main";

static char args_doc[] = "[FILENAME]...";

static struct argp_option options[] = {
    { "hybrid", 'h', "MODE", 0, "Enable hybrid mode." },
    { "debug", 'd', 0, 0, "Enable debug mode" },
    { 0 }
};

struct arguments {
  enum { TJ, MJ, OFF } hybrid_mode;
  bool debug;
};

static error_t parse_opt(int key, char *arg, struct argp_state *state) {
  struct arguments *arguments = state->input;
  switch (key) {
  case 'h':
    if (strcmp(arg, "tj") == 0 || strcmp(arg, "tracing") == 0) {
      arguments->hybrid_mode = TJ;
    } else if (strcmp(arg, "mj") == 0 || strcmp(arg, "method") == 0) {
      arguments->hybrid_mode = MJ;
    } else {
      arguments->hybrid_mode = OFF;
    }
    break;
  case 'd': arguments->debug = true; break;
  case ARGP_KEY_ARG: return 0;
  default: return ARGP_ERR_UNKNOWN;
  }
  return 0;
}

static struct argp argp = { options, parse_opt, args_doc, doc, 0, 0, 0 };

int main(int argc, char *argv[]) {
  char *hp, *sp;
  struct arguments arguments;

  arguments.hybrid_mode = NORMAL;
  arguments.debug = false;

  argp_parse(&argp, argc, argv, 0, 0, &arguments);

  switch(arguments.hybrid_mode) {
  case TJ:
    jit_mode = HYBRID_TJ;
    //puts("hybrid mode (tj)");
    break;
  case MJ:
    jit_mode = HYBRID_MJ;
    //puts("hybrid mode (mj)");
    break;
  case OFF:
    jit_mode = NORMAL;
    break;
  default:
    jit_mode = NORMAL;
    break;
  }

  caml_main(argv);

  min_caml_stderr = stderr;
  sp = alloca(1000000); hp = malloc(40000000);
  if (hp == NULL || sp == NULL) {
    fprintf(stderr, "malloc or alloca failed\n");
    return 1;
  }
  /* fprintf(stderr, "sp = %p, hp = %p\n", sp, hp); */
  min_caml_start(sp, hp);

  return 0;
}
