#ifndef RUNTIME_H_
#define RUNTIME_H_

#include <stdbool.h>
#include <stdint.h>

enum jit_mode { NORMAL, HYBRID_TJ, HYBRID_MJ, HYBRID_ALL };

extern bool no_jit;

void set_jit_mode(enum jit_mode);

void c_can_enter_jit(long *, long, long *, long);

void c_jit_setup(long *, long, long *, long);

void c_jit_merge_point(long*, long, long*, long);

#endif
