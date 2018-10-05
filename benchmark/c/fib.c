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
  int res, n, N;
  n = 40;
  N = 50;
  float start = get_micro_time();
  for (int i = 0; i < N; i++) {
    res = fib(n);
  }
  float end = get_micro_time();
  float ellapse = (end - start) / (int)1e6;
  printf("fib(%d): %f s\n", n, ellapse / N);
}
