#define _GNU_SOURCE

#include <assert.h>
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

#include "interop_caml.h"
#include "runtime_camlwrap.h"

#define ARR_LEN 2048
#define THOLD_TJ 100
#define THOLD_MJ 0

#define JIT_COMPILE_COMMAND "gcc -m32 -fPIC -shared"

typedef int (*fun_arg2)(int*, int);

enum jit_type { TJ, MJ };

/**
 * For profiling a program counter
 */
int prof_arr[ARR_LEN] = {0};

bool compiled_arr[ARR_LEN] = {false};

char* trace_name_arr[ARR_LEN] = {NULL};

fun_arg2 sym_arr[ARR_LEN] = {NULL};

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
void gen_so_name(char* buf, char *trace_name) {
  char str1[1024] = "./lib";
  char ext[12] = ".so";
  strcat(str1, trace_name); // "./libtracetj0.123"
  strcat(str1, ext); // "./libtracetj0.123.so"
  strcpy(buf, str1);
  return;
}

void strip_ext(char *fname) {
  char *end = fname + strlen(fname);

  while (end > fname && *end != '.') {
    --end;
  }

  if (end > fname) {
    *end = '\0';
  }
}

/**
 * Compile a trace into shared object
 */
void jit_compile(char *so, char *func) {
  char buffer[1024];

  printf("compiling trace %s into shared object %s\n", func, so);
  sprintf(buffer, "%s -c %s.s", JIT_COMPILE_COMMAND, func);
  system(buffer);

  sprintf(buffer, "%s -shared -rdynamic -o %s %s.o", JIT_COMPILE_COMMAND, so,
          func);
  system(buffer);

  return;
}



/**
 * Profiling how many back-edge insertions occur.
 */
void c_can_enter_jit(int *stack, int sp, int *code, int pc) {
  //printf("back edge at pc %d\n", pc);
  if (pc != 96)
    prof_arr[pc]++;
  return;
}

/**
 * Entry point of jitting.
 * TODO: change the arguments of jit_merge_point in interp.mcml
 */
void c_jit_merge_point(int* stack, int sp, int* code, int pc) {
  int pc_count;
  char trace_name[128];
  char so_name[1024];
  void* handle = NULL;
  fun_arg2 sym = NULL;

  pc_count = prof_arr[pc];
  if (pc_count < THOLD_TJ) {
    // exit if pc_count is under THOLD
    return;
  } else {
    //printf("over thold at pc %d\n", pc);
    //call_caml_jit_entry(stack, sp, code, pc);

    if (!compiled_arr[pc] ) {
      strcpy(trace_name, call_caml_jit_tracing(stack, sp, code, pc));
      trace_name_arr[pc] = malloc(128*sizeof(char));
      strcpy(trace_name_arr[pc], trace_name);

      gen_so_name(so_name, trace_name);
      jit_compile(so_name, trace_name);
      compiled_arr[pc] = true;
    }

    if (sym_arr[pc] == NULL) {
      strcpy(trace_name, trace_name_arr[pc]);
      gen_so_name(so_name, trace_name);

      strip_ext(trace_name);
      handle = dlopen(so_name, RTLD_NOW | RTLD_GLOBAL);
      if (handle == NULL) {
        fprintf(stderr, "error: dlopen %s\n", so_name);
        exit(-1);
      }
      dlerror();

      sym = (fun_arg2)dlsym(RTLD_DEFAULT, trace_name);
      if (sym == NULL) {
        fprintf(stderr, "error: dlsym \n");
        return;
      }
      sym_arr[pc] = malloc(sizeof(fun_arg2));
      sym_arr[pc] = sym;
      sym(stack, sp);
      printf("execution finished at pc %d\n", pc);
      return;
    } else {
      sym = sym_arr[pc];
      sym(stack, sp);
      return;
    }
  }
}
