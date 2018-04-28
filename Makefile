.default: build

.all: build clean example testall runtest gcc

CC = gcc
CFLAGS = -g -O2 -Wall
OCAMLLDFLAGS = -warn-error -31
OCAMLBUILD_OPTIONS = -use-ocamlfind
COMPILER = min-caml

EXAMPLES = print sum-tail gcd sum fib ack even-odd adder \
funcomp cls-rec cls-bug cls-bug2 cls-reg-bug shuffle \
spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 inprod inprod-rec \
inprod-loop matmul matmul-flat manyargs fib-tail array array2 \
float tuple

MAIN := src/main.exe

.PHONY: build
build:
	jbuilder build $(MAIN)
	ln -s _build/default/$(MAIN) .

.PHONY: clean
clean:
	jbuilder clean

.PHONY: jclean
jit_clean:
	@rm -rf *.o *.s test/*.o test/*.s
	@rm -rf *.dSYM

.PHONY: test
runtest:
	jbuilder runtest

.PHONY: gcc
gcc:
	./min-caml test/data/$(INTERP) && \
	gcc -c -m32 $(TRACE).s && \
	gcc -g -Wall -O2 -m32 lib/libmincaml.S lib/stub.c test/data/$(INTERP).s $(TRACE).o -o $(TRACE) && \
	rm -rf $(TRACE).dSYM

.PHONY: example
example: $(EXAMPLES:%=example/%.cmp)

.PRECIOUS: example/%.s example/% example/%.res example/%.ans example/%.cmp
TRASH = $(EXAMPLES:%=example/%.s) $(EXAMPLES:%=example/%) $(EXAMPLES:%=example/%.res) $(EXAMPLES:%=example/%.ans) $(EXAMPLES:%=example/%.cmp)

example/%.s: compiler example/%.ml
	./$(COMPILER) example/$*
example/%: example/%.s lib/libmincaml.S lib/stub.c
	$(CC) $(CFLAGS) -m32 $^ -lm -o $@
example/%.res: example/%
	$< > $@
example/%.ans: example/%.ml
	ocaml $< > $@
example/%.cmp: example/%.res example/%.ans
	diff $^ > $@
