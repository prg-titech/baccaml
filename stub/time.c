#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>

int min_caml_get_micro_time() asm ("min_caml_get_micro_time");
void min_caml_sleep() asm ("min_caml_sleep");

// sleep in 1 second
void min_caml_sleep() {
  sleep(1);
  return;
}

// getting time in micro seconds
int min_caml_get_micro_time() {
  struct timeval current_time;
  gettimeofday(&current_time, NULL);
  return current_time.tv_sec * (int)1e6 + current_time.tv_usec;
}
