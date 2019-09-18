APP = fib
APPS = fib sum

STUB = ../../stub/stub.c
LIBMINCAML = ../../stub/libmincaml.S

CC = gcc
CFLAGS = -m32 -lm

MINCAML = min-caml

default: $(APP)

$(APP): $(APP).s $(STUB) $(LIBMINCAML)
	$(CC) $(CFLAGS) -I`ocamlc -where` $^ -o $@

%.s: %.ml
	$(MINCAML) $(basename $<)

clean:
	rm -f *.o *.cm* $(APPS)

clean-all: clean
	rm -f *.s
