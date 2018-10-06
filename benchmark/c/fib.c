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
  int x;
  x = fib(40);
  return 0;
}
