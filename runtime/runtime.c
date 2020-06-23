#define _GNU_SOURCE

#include <assert.h>
#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/compatibility.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <dlfcn.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <unistd.h>

#define PROF_LEN 2048
#define THOLD_TJ 100
#define THOLD_MJ 100

#define JIT_COMPILE_COMMAND "gcc -m32 -fPIC -shared"

typedef int (*fun_arg2)(int, int);

enum jit_type { TJ, MJ };

/**
 * For gen_trace_name
 */
int counter_name = 0;

/**
 * Generate the name of a trace
 * - tracing JIT: tracetj0, tracetj1, ...
 * - method JIT: tracemj0, tracemj1, ...
 */
void gen_trace_name(char *buffer, enum jit_type typ) {
  if (typ == TJ) {
    sprintf(buffer, "tracetj%d", counter_name);
  } else if (typ == MJ) {
    sprintf(buffer, "tracemj%d", counter_name);
  } else {
    fprintf(stderr, "invalid jit_type");
    exit(EXIT_FAILURE);
  }
  counter_name += 1;
  return;
}

/**
 * Generate a name for created shared objects
 */
void gen_so_name(char *buffer, char *tname) {
  sprintf(buffer, "./lib%s.so", tname);
}

/**
 * Compile a trace into shared object
 */
void jit_compile(char *so, char *func, int pc) {
  char buffer[1024];

  printf("compiling shared object: %s\n", so);
  sprintf(buffer, "%s -c %s.s", JIT_COMPILE_COMMAND, func);
  system(buffer);

  sprintf(buffer, "%s -shared -rdynamic -o %s %s.o", JIT_COMPILE_COMMAND, so,
          func);
  system(buffer);

  return;
}
