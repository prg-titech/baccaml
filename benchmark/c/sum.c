#include <stdio.h>
#include "time.h"

int sum(int n) {
  if (n == 0) return 0;
  else return n + sum(n - 1);
}

int main() {
  int x, i, n, start, end, iter;
  iter = 10;
  n = 5000;
  i = 0;
  start = get_micro_time();
  while(i < iter) {
    x = sum(n);
    i++;
  }
  end = get_micro_time();
  printf("%f us\n", (end - start) / (float)iter);
  return 0;
}
