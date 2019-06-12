#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <caml/mlvalues.h>
#include <caml/callback.h>

extern void call_caml_jit_entry(int *, int , int *, int) asm ("call_caml_jit_entry");

extern void call_caml_jit_exec(int, int *, int) asm ("call_caml_jit_exec");

extern void call_caml_jit_mj_call(int *, int , int *, int) asm ("call_caml_mj_call");

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

int call_caml_mj_call(int *st, int sp, int *bc, int pc) {
  static value * jit_mj_call_closure = NULL;
  value ml_args[6];
  if (jit_mj_call_closure == NULL) {
    jit_mj_call_closure = caml_named_value("jit_mj_call");
  }
  ml_args[0] = caml_alloc_array(init_f, bc);
  ml_args[1] = caml_alloc_array(init_f, st);
  ml_args[2] = Val_int(pc);
  ml_args[3] = Val_int(sp);
  ml_args[4] = Val_hp(bc);
  ml_args[5] = Val_hp(st);
  return Int_val(caml_callbackN(*jit_mj_call_closure, 6, ml_args));
}
