APP = fib
CFLAGS = -m32 -O2 -Wall -lm

default: $(APP)

$(APP): $(APP).ml
	min-caml $(basename $<); \
	gcc $(CFLAGS) stub.c libmincaml.S $(basename $<).s -o $(basename $<)

test: test_time.ml
	min-caml $(basename $<); \
	gcc $(CFLAGS) stub.c libmincaml.S $(basename $<).s -o $(basename $<)

clean:
	rm -f *.o *.cm* $(APP)

clean-all: clean
	rm -f *.s
