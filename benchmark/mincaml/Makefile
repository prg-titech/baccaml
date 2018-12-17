APP = fib

default: $(APP)

$(APP): $(APP).ml
	min-caml $(basename $<); \
	gcc -m32 ../../stub/stub.c ../../stub/libmincaml.S $(basename $<).s -o $(basename $<)

test: test_time.ml
	min-caml $(basename $<); \
	gcc -m32 ../../stub/stub.c ../../stub/libmincaml.S $(basename $<).s -o $(basename $<)

clean:
	rm -f *.o *.cm*

clean-all: clean
	rm -f *.s
