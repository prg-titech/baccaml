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
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#include "interop_caml.h"
#include "runtime_camlwrap.h"

#define ARR_LEN 2048
#define THOLD_TJ 10
#define THOLD_MJ 0
//#define TIME_IT

#define JIT_COMPILE_COMMAND "gcc -m32 -fPIC -shared"

typedef int (*fun_arg2)(int*, int);

enum jit_type { TJ, MJ };

#ifdef CLOCK_PROCESS_CPUTIME_ID
/* cpu time in the current process */
#define CLOCKTYPE  CLOCK_PROCESS_CPUTIME_ID
#else
/* this one should be appropriate to avoid errors on multiprocessors systems */
#define CLOCKTYPE  CLOCK_MONOTONIC
#endif

/**
 * Estimate elapsed time (us).
 */
double time_it(int (*action)(int*, int), int* arg1, int arg2) {
  struct timespec tsi, tsf;

  clock_gettime(CLOCKTYPE, &tsi);
  int r = action(arg1, arg2);
  clock_gettime(CLOCKTYPE, &tsf);

  double elaps_s = difftime(tsf.tv_sec, tsi.tv_sec);
  long elaps_ns = tsf.tv_nsec - tsi.tv_nsec;
  fprintf(stderr, "execution time %10f us\n", elaps_s + ((double)elaps_ns) / 1.0e3);
  return r;
}

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

void concat(char* buf, char** arr, int size) {
  strcpy(buf, "");

  for (int i = 0; i < size; i++) {
    strcat(buf, arr[i]);
    strcat(buf, ".o");
    strcat(buf, " ");
  }
}

void chars_of_value(char *buf[], value deps, int size) {
  for (int i = 0; i < size; i++) {
    char* str = String_val(Field(deps, i));
    buf[i] = malloc(128 * sizeof(char));
    strcpy(buf[i], strdup(str));
  }
}

/**
 * Compile a trace into shared object
 */
void jit_compile(char *so, char *func) {
  char buffer[1024];

  sprintf(buffer, "%s -c %s.s", JIT_COMPILE_COMMAND, func);
  system(buffer);

  sprintf(buffer, "%s -rdynamic -DRUNTIME -o %s %s.o", JIT_COMPILE_COMMAND, so, func);
  system(buffer);

  return;
}

/**
 * Compile a trace with other shared libraries
 */
void jit_compile_with_sl(char *so, char *func, char **arr, int size) {
  char buffer[1024];
  char other_sl[1024];

  concat(other_sl, arr, size);

  sprintf(buffer, "%s -c %s.s", JIT_COMPILE_COMMAND, func);
  system(buffer);

  sprintf(buffer, "%s -rdynamic -DRUNTIME -L. -I. -o %s %s %s.o ",
          JIT_COMPILE_COMMAND, so, other_sl, func);
  system(buffer);
}

int c_mj_call(int *stack, int sp, int *code, int pc) {
  char trace_name[128]; char so_name[128];
  char* deps[10];
  value v;
  void* handle = NULL;
  fun_arg2 sym = NULL;


  if (!compiled_arr[pc]) {
    v = call_caml_jit_method(stack, sp, code, pc);
    strcpy(trace_name, strdup(String_val(Field(v, 0))));
    trace_name_arr[pc] = malloc(128*sizeof(char));
    strcpy(trace_name_arr[pc], trace_name);

    int d_size = Int_val(Field(v, 2));
    chars_of_value(deps, Field(v, 1), d_size);

    gen_so_name(so_name, trace_name);

    if (d_size == 0) {
      fprintf(stderr, "compiling trace %s into %s at pc %d\n", trace_name, so_name, pc);
      jit_compile(so_name, trace_name);
    } else if (d_size > 0) {
      jit_compile_with_sl(so_name, trace_name, deps, d_size);
    } else {
      fprintf(stderr, "value of dependency-size is not valid.\n");
      exit(-1);
    }

    compiled_arr[pc] = true;
  }

  if (sym_arr[pc] == NULL) {
    strcpy(trace_name, trace_name_arr[pc]);
    gen_so_name(so_name, trace_name);

    strip_ext(trace_name);
    handle = dlopen(so_name, RTLD_NOW);
    if (handle == NULL) {
      fprintf(stderr, "error: dlopen %s\n", so_name);
      exit(-1);
    }
    dlerror();

    sym = (fun_arg2)dlsym(handle, trace_name);
    if (sym == NULL) {
      fprintf(stderr, "error: dlsym \n");
      exit(-1);
    }
    sym_arr[pc] = malloc(sizeof(fun_arg2));
    sym_arr[pc] = sym;

    int r = time_it(sym, stack, sp);
    return r;
  } else {
    fun_arg2 sym = sym_arr[pc];
    int r = time_it(sym, stack, sp);
    return r;
  }
}

/**
 * Profiling how many back-edge insertions occur.
 */
void c_can_enter_jit(int *stack, int sp, int *code, int pc) {
  if (pc!=96) prof_arr[pc]++;
  return;
}

/**
 * Entry point of jitting.
 * TODO: change the arguments of jit_merge_point in interp.mcml
 */
void c_jit_merge_point(int* stack, int sp, int* code, int pc) {
  char trace_name[128];
  char so_name[128];
  void* handle = NULL;
  fun_arg2 sym = NULL;
  value v;
  char* deps[10];
  int d_size;

  int pc_count = prof_arr[pc];
  if (pc_count < THOLD_TJ) {
    // exit if pc_count is under THOLD
    return;
  } else {
    //printf("over thold at pc %d\n", pc);
    //call_caml_jit_entry(stack, sp, code, pc);

    if (!compiled_arr[pc]) {
      // if not compiled, compile the trace and mark `already compiled'
      v = call_caml_jit_tracing(stack, sp, code, pc); // string, string array, int

      strcpy(trace_name, strdup(String_val(Field(v, 0))));
      trace_name_arr[pc] = malloc(128*sizeof(char));
      strcpy(trace_name_arr[pc], trace_name);

      d_size = Int_val(Field(v, 2));
      chars_of_value(deps, Field(v, 1), d_size);

      gen_so_name(so_name, trace_name);

      if (d_size == 0) {
        jit_compile(so_name, trace_name);
      } else if (d_size > 0) {
        jit_compile_with_sl(so_name, trace_name, deps, d_size);
      } else {
        fprintf(stderr, "value of dependency-size is not valid.\n");
        exit(-1);
      }

      compiled_arr[pc] = true;
    }

    //printf("executing at pc %d\n", pc);
    if (sym_arr[pc] == NULL) {
      strcpy(trace_name, trace_name_arr[pc]);
      gen_so_name(so_name, trace_name);

      strip_ext(trace_name);
      handle = dlopen(so_name, RTLD_NOW);
      if (handle == NULL) {
        fprintf(stderr, "error: dlopen %s\n", so_name);
        exit(-1);
      }
      dlerror();

      sym = (fun_arg2)dlsym(handle, trace_name);
      if (sym == NULL) {
        fprintf(stderr, "error: dlsym \n");
        exit(-1);
      }
      sym_arr[pc] = malloc(sizeof(fun_arg2));
      sym_arr[pc] = sym;
#if defined (TIME_IT)
      time_it(sym, stack, sp);
#else
      sym(stack, sp);
#endif
      return;
    } else {
      sym = sym_arr[pc];
#if defined (TIME_IT)
      time_it(sym, stack, sp);
#else
      sym(stack, sp);
#endif
      return;
    }
  }
}
