#ifndef RUNTIME_H
#define RUNTIME_H

#include <stdbool.h>

enum jit_mode { HYBRID_TJ, HYBRID_MJ, NORMAL };

static enum jit_mode jit_mode;

bool no_jit;

#endif
