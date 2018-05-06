.default: build

.all: build clean jit-clean example test test-one gcc

CC = gcc
CFLAGS = -g -O2 -Wall
OCAMLLDFLAGS = -warn-error -31

MAIN = main.exe
COMPILER = min-caml

EXAMPLES = print sum-tail gcd sum fib ack even-odd adder \
funcomp cls-rec cls-bug cls-bug2 cls-reg-bug shuffle \
spill spill2 spill3 join-stack join-stack2 join-stack3 \
join-reg join-reg2 non-tail-if non-tail-if2 inprod inprod-rec \
inprod-loop matmul matmul-flat manyargs fib-tail array array2 \
float tuple

.PHONY: build
build:
	@jbuilder build src/$(MAIN)
	@ln -s _build/default/src/$(MAIN) .
	@mv $(MAIN) $(COMPILER)

.PHONY: clean
clean:
	@jbuilder clean
	@rm -rf $(TRASH)

.PHONY: jit-clean
jit-clean:
	@rm -rf *.o *.s test/*.o test/*.s *.dSYM

.PHONY: dump-clean
dump-clean:
	@rm -rf **/*.dump

.PHONY: clean-all
clean-all: clean jit-clean

.PHONY: test
test:
	@jbuilder runtest

.PHONY: test-one
test-one:
	@jbuilder build test/$(SPEC).exe
	@cd _build/default/test || exit 1 && ./$(SPEC).exe

.PHONY: gcc
gcc:
	@./$(COMPILER) test/$(INTERP)
	@gcc -c -m32 _build/default/test/$(TRACE).s -o _build/default/test/$(TRACE).o
	@gcc -g -Wall -O2 -m32 lib/libmincaml.S lib/stub.c test/$(INTERP).s _build/default/test/$(TRACE).o -o $(TRACE)
	@rm -rf $(TRACE).dSYM

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
