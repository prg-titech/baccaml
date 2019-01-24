#include<string.h>
#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
#include<math.h>
#include"csyntax.h"

static inline Value* make_int_array(int size, int initial_value){
  int x;
  Value* array = NULL;  
  if(size < 1){
    size = 1;
  }
  array = (Value*) malloc(size * sizeof(Value));
  if(array == NULL){
    printf("Error allocating memory for array\n");
    exit(-1);
  }
  for(x = 0; x < size; x++){
    array[x].i = initial_value;
  }
  return array;
}

static inline Value* make_double_array(int size, double initial_value){
  int x;
  Value* array = NULL;  
  if(size < 1){
    size = 1;
  }
  array = (Value*) malloc(size * sizeof(Value));
  if(array == NULL){
    printf("Error allocating memory for array\n");
    exit(-1);
  }
  for(x = 0; x < size; x++){
    array[x].d = initial_value;
  }
  return array;
}

static inline Value* make_multi_array(int size, Value* initial_value){
  int x;
  Value* array = NULL;  
  if(size < 1){
    size = 1;
  }
  array = (Value*) malloc(size * sizeof(Value));
  if(array == NULL){
    printf("Error allocating memory for array\n");
    exit(-1);
  }
  for(x = 0; x < size; x++){
    array[x].a = initial_value;
  }
  return array;
}

static inline Value* make_closure_array(int size, Closure* initial_value){
  int x;
  Value* array = NULL;  
  if(size < 1){
    size = 1;
  }
  array = (Value*) malloc(size * sizeof(Value));
  if(array == NULL){
    printf("Error allocating memory for array\n");
    exit(-1);
  }
  for(x = 0; x < size; x++){
    array[x].c = initial_value;
  }
  return array;
}

static inline Closure* closure_malloc(){
    Closure* c = NULL;
    c = (Closure*) malloc(sizeof(Closure));
    if(c == NULL){
	printf("Error allocating memory for closure\n");
	exit(-1);
    }
    return c;
}


static inline Value* safe_malloc(int size){
  Value* env = NULL;
  if(size > 0){
      env = (Value*) malloc(size * sizeof(Value));
    if(env == NULL){
      printf("Error allocating memory for environment\n");
      exit(-1);
    }
  }
  return env;
}
