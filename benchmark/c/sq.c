#include <stdio.h>
#include "time.h"

int sq(int n) {
  int m = n;
  for(int i = 1; i < n; i++)  {
    m += n;
  }
  return m;
}

int main() {
  int iter = 100;
  int i = 0;
  int s = get_micro_time();
  while (i < iter) {
    int res = sq(500000);
    i++;
  }
  int e = get_micro_time();
  printf("%d\n", (e - s));
  return 0;
}
