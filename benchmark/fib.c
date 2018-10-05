#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>

int get_micro_time() {
  struct timeval current_time;
  gettimeofday(&current_time, NULL);
  return current_time.tv_sec * (int)1e6 + current_time.tv_usec;
}

int fib(int n) {
  if (n < 2) {
    return 1;
  } else {
    return fib(n - 1) + fib(n - 2);
  }
}

int main() {
  int res;
  int start = get_micro_time();
  res = fib(30);
  int end = get_micro_time();
  printf("fib(30): %d\n", (end - start));
}
