#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>

int get_micro_time();

// getting time in micro seconds
int get_micro_time() {
  struct timeval current_time;
  gettimeofday(&current_time, NULL);
  return current_time.tv_sec * (int)1e6 + current_time.tv_usec;
}
