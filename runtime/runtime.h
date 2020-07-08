#ifndef RUNTIME_H_
#define RUNTIME_H_

#include <stdbool.h>

enum jit_mode { NORMAL, HYBRID_TJ, HYBRID_MJ };

extern bool no_jit;

void set_jit_mode(enum jit_mode);

void c_can_enter_jit(int *, int, int *, int);

void c_jit_setup(int *, int, int *, int);

void c_jit_merge_point(int*, int, int*, int);

#endif
