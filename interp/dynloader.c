#include <stdio.h>
#include <stdint.h>
#include <dlfcn.h>
#include <caml/mlvalues.h>

typedef int (*fun_arg2)(int*, int);

typedef int (*fun_arg1)(int);

CAMLprim value call_dlfun_arg1(value filename, value funcname, value arg1) {
  fun_arg1 sym = NULL;
  void *handle = NULL;

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

  return Val_int(sym(Int_val(arg1)));
}

CAMLprim value call_dlfun_arg2(value filename, value funcname, value arg1, value arg2) {
  fun_arg2 sym = NULL;
  void *handle = NULL;

  handle = dlopen(String_val(filename), RTLD_LAZY);
  if (handle == NULL) {
    char s[100];
    sprintf(s, "dlopen error: %s, %s", String_val(filename), String_val(funcname));
    failwith(s);
    return -1;
  }

  sym = (fun_arg2)dlsym(handle, String_val(funcname));
  if (sym == NULL) {
    failwith("error: dlsym\n");
    return -1;
  }

  int *stk = (int *)(Int_val(arg1) << 2);
  int sp = Int_val(arg2);

  return Val_int(sym(stk, sp));
}

int call_dlfun(char* filename, char* funcname, int* stk, int st_ptr) {
  fun_arg2 sym = NULL;
  void *handle = NULL;

  handle = dlopen(filename, RTLD_LAZY);
  if (handle == NULL) {
    dlerror();
    return -1;
  }

  sym = (fun_arg2)dlsym(handle, funcname);
  if (sym == NULL) {
    dlerror();
    return -1;
  }

  return sym(stk, st_ptr);
}
