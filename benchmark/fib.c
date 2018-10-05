#include <stdio.h>
#include "time.h"

int fib(int n) {
  if (n < 2) {
    return 1;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

int main() {
  int res, n;
  n = 40;
  float start = get_micro_time();
  res = fib(n);
  float end = get_micro_time();
  printf("fib(%d): %f s\n", n, (end - start) / (int)1e6);
}
