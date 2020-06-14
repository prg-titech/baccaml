#include "caml/compatibility.h"
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <dlfcn.h>
#include <stdint.h>
#include <stdio.h>

typedef int (*fun_arg3)(int *, int, int *);

typedef int (*fun_arg2)(intptr_t, int);

typedef int (*fun_arg1)(int);

CAMLprim value call_dlfun_arg1(value filename, value funcname, value arg1) {
  fun_arg1 sym = NULL;
  void *handle = NULL;
  int res = 0;

  handle = dlopen(String_val(filename), RTLD_LAZY);
  if (handle == NULL) {
    failwith("error: dlopen\n");
    return -1;
  }

  sym = (fun_arg1)dlsym(handle, String_val(funcname));
  if (sym == NULL) {
    failwith("error: dlsym\n");
    return -1;
  }
  res = sym(arg1);
  dlclose(handle);
  return Val_int(res);
}

CAMLprim value call_dlfun_arg2(value filename, value funcname, value arg1,
                               value arg2) {
  fun_arg2 sym = NULL;
  void *handle = NULL;
  int res = 0;
  char* name = String_val(filename);

  handle = dlopen(name, RTLD_LAZY);
  if (handle == NULL) {
    char s[100];
    sprintf(s, "dlopen error: %s, %s", name,
            String_val(funcname));
    failwith(s);
    return -1;
  }

  dlerror();

  sym = (fun_arg2)dlsym(handle, String_val(funcname));
  if (sym == NULL) {
    char msg[100];
    sprintf(msg, "error: dlsym funcname: %s\n", String_val(funcname));
    failwith(msg);
    return -1;
  }

  intptr_t stk = Hp_val(arg1);
  int sp = Int_val(arg2);

  res = sym(stk, sp);
  // dlclose(handle); // too slow

  return Val_int(res);
}

CAMLprim value call_dlfun_arg3(value filename, value funcname, value arg1,
                               value arg2, value arg3) {
  fun_arg3 sym = NULL;
  void *handle = NULL;

  handle = dlopen(String_val(filename), RTLD_LAZY);
  if (handle == NULL) {
    char s[100];
    sprintf(s, "dlopen error: %s, %s", String_val(filename),
            String_val(funcname));
    failwith(s);
    return -1;
  }

  sym = (fun_arg3)dlsym(handle, String_val(funcname));
  if (sym == NULL) {
    failwith("error: dlsym\n");
    return -1;
  }

  int *stk = Hp_val(arg1);
  int sp = Int_val(arg2);
  int *btk = Hp_val(arg3);

  return Val_int(sym(stk, sp, btk));
}
